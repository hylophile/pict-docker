
open Ccode

let externUses = (Hashtbl.create 128 : (var,int ref) Hashtbl.t)
let uses = (Hashtbl.create 64 : (var,int ref * int ref) Hashtbl.t)

let addUsage x =
  try incr (fst (Hashtbl.find uses x)) with Not_found ->
  try incr (Hashtbl.find externUses x) with Not_found ->
  Hashtbl.add uses x (ref 1,ref 0)

let addInit x =
  try incr (snd (Hashtbl.find uses x)) with Not_found ->
  Hashtbl.add uses x (ref 0,ref 1)

let getUses x =
  try !(fst (Hashtbl.find uses x)) with Not_found -> 0

let getInits x =
  try !(snd (Hashtbl.find uses x)) with Not_found -> 0

let rec impureExp = function
  OFFSET(m,e,_) -> m or impureExp e
| DEREF(_,_) -> true
| CCODE(ci,_,l) ->
    ci.Ccode.reader || ci.Ccode.writer || ci.Ccode.alloc > 0 ||
    List.exists impureExp l
| CCALL(ci,_,l) ->
    ci.Ccode.reader || ci.Ccode.writer || ci.Ccode.alloc > 0 ||
    List.exists impureExp l
| _ -> false

let rec eliminableExp = function
  OFFSET(_,e,_) -> eliminableExp e
| CCODE(ci,_,l) -> (not ci.Ccode.writer) && List.for_all eliminableExp l
| CCALL(ci,_,l) -> (not ci.Ccode.writer) && List.for_all eliminableExp l
| _ -> true

let rec costlyExp = function
  OFFSET(_,_,_) -> true
| DEREF(_,_) -> true
| CCODE(_,_,_) -> true
| CCALL(_,_,_) -> true
| _ -> false

let rec scan = function
  SEQ(c,d) -> scan c; scan d
| IF(e,c1,c2) -> scanExp e; scan c1; scan c2
| ASSIGN(VAR x,e) when not(eliminableExp e) -> addInit x; addUsage x; scanExp e
| ASSIGN(VAR x,e) -> addInit x; scanExp e
| ASSIGN(e,f) -> scanExp e; scanExp f
| CODE(_,_,l) -> List.iter scanExp l
| CALL(_,_,l) -> List.iter scanExp l
| PROFILING(_) -> ()
| NULL -> ()

and scanExp = function
  VAR(x) -> addUsage x
| TAG(x,_) -> addUsage x
| INDEX(x,e) -> addUsage x; scanExp e
| DEREF(x,_) -> addUsage x
| OFFSET(_,e,_) -> scanExp e
| CCODE(_,_,l) -> List.iter scanExp l
| CCALL(_,_,l) -> List.iter scanExp l
| ADDR(x) -> addUsage x
| TAGS(x) -> addUsage x
| _ -> ()

let scanDecl = function
  EXTERN(x) -> Hashtbl.add externUses x (ref 0)
| _ -> ()

let info = (Hashtbl.create 64 : (var,exp) Hashtbl.t)

let lookupInfo x = try Hashtbl.find info x with Not_found -> VAR x

let rec inline = function
  SEQ(c,d) -> let c = inline c in SEQ(c,inline d)
| IF(e,c1,c2) -> IF(inlineExp e,inline c1,inline c2)
| ASSIGN(e,f) ->
    (match inlineExp e with
       (VAR x) as e ->
         let f = inlineExp f in 
	 if
	   (costlyExp f && getUses x > 1) || impureExp f || getInits x > 1
	 then
	   ASSIGN(e,f)
	 else
	   (Hashtbl.add info x f; ASSIGN(e,f))
     | e -> ASSIGN(e,inlineExp f))
| CODE(ci,sl,l) -> CODE(ci,sl,List.map inlineExp l)
| CALL(ci,s,l) -> CALL(ci,s,List.map inlineExp l)
| PROFILING(_) as c -> c
| NULL -> NULL

and inlineExp = function
  (VAR x) as e ->
    lookupInfo x
| TAG(x,i) as e ->
    (match lookupInfo x with
       VAR v -> TAG(v,i)
     | INDEX(v,INT j) -> TAG(v,i+j)
     | _ -> e)
| INDEX(x,INT i) as e ->
    (match lookupInfo x with
       VAR v -> INDEX(v,INT i)
     | INDEX(v,INT j) -> INDEX(v,INT(i+j))
     | _ -> e)
| DEREF(x,i) as e ->
    (match lookupInfo x with
       VAR v -> DEREF(v,i)
     | INDEX(v,INT j) -> DEREF(v,i+j)
     | _ -> e)
| OFFSET(m,e,i) ->
    (match inlineExp e with
       VAR v -> OFFSET(m,VAR v,i)
     | TAG(v,j) -> DEREF(v,i+j)
     | e -> OFFSET(m,e,i))
| CCODE(ci,sl,l) ->
    CCODE(ci,sl,List.map inlineExp l)
| CCALL(ci,s,l) ->
    CCALL(ci,s,List.map inlineExp l)
| e -> e

let inlineDecl = function
  BLOCK(var,l,free,endq,i,c) ->
    Hashtbl.clear info; Hashtbl.clear uses; scan c;
    BLOCK(var,l,free,endq,i,inline c)
| CONST(_,l) as d -> List.iter scanExp l; d
| d -> d

let optimiseVar l (x,ty) = if getUses x > 0 then (x,ty) :: l else l

let rec optimise = function
  SEQ(c,d) ->
  (match optimise c with NULL -> optimise d | c ->
   match optimise d with NULL -> c | d -> SEQ(c,d))
| IF(e,c1,c2) -> IF(e,optimise c1,optimise c2)
| ASSIGN(VAR x,f) as c -> if getUses x = 0 then NULL else c
| c -> c

let optimiseDecl d l = match d with
  TOPLEVEL(_) | CONST(_) | BYTES(_) | FORWARD(_) -> d :: l
| EXTERN(x) -> if !(Hashtbl.find externUses x) > 0 then d :: l else l
| BLOCK(x,v,free,endq,i,c) ->
  Hashtbl.clear uses; scan c;
  if i > 0 then (addUsage free; addUsage endq);
  BLOCK(x,List.fold_left optimiseVar [] v,free,endq,i,optimise c) :: l

let next = ref 0
let table = (Hashtbl.create 64 : (var,var) Hashtbl.t)

let renameLocal x =
  let v = Var.indexed !next in
  incr next; Hashtbl.add table x v; v

let renameVar x =
  try Hashtbl.find table x with Not_found -> x

let rec rename = function
  SEQ(c,d) -> SEQ(rename c,rename d)
| IF(e,c1,c2) -> IF(renameExp e,rename c1,rename c2)
| ASSIGN(e,f) -> ASSIGN(renameExp e,renameExp f)
| CODE(ci,sl,l) -> CODE(ci,sl,List.map renameExp l)
| CALL(ci,s,l) -> CALL(ci,s,List.map renameExp l)
| PROFILING(_) as c -> c
| NULL -> NULL

and renameExp e = match e with
  VAR(x) -> VAR(renameVar x)
| TAG(x,i) -> TAG(renameVar x,i)
| INDEX(x,e) -> INDEX(renameVar x,renameExp e)
| DEREF(x,i) -> DEREF(renameVar x,i)
| OFFSET(m,e,i) -> OFFSET(m,renameExp e,i)
| CCODE(ci,sl,l) -> CCODE(ci,sl,List.map renameExp l)
| CCALL(ci,s,l) -> CCALL(ci,s,List.map renameExp l)
| COERCION(c,e) -> COERCION(c,renameExp e)
| INT(_) | ADDR(_) | TAGS(_) -> e

let renameDecl d = match d with
  TOPLEVEL(_) | CONST(_) | BYTES(_) | FORWARD(_) | EXTERN(_) -> d
| BLOCK(x,v,free,endq,i,c) ->
  Hashtbl.clear table; next := 0;
  let v = List.map (fun (x,ty) -> (renameLocal x,ty)) v in
  BLOCK(x,v,renameVar free,renameVar endq,i,rename c)

let optimiseInstr =
  Flags.createFlag "optimiseInstr"
  "*Optimise C instructions before calling the C compiler" true
let showOptimisedInstr =
  Flags.createFlag "showOptimisedInstr"
  "*Show C instructions after optimisation" false
let renameLocals =
  Flags.createFlag "renameLocals"
  "*Rename local variables before calling the C compiler" true
    
let optimise l =
  if !optimiseInstr then
    begin
      Stats.mark "Instruction optimisation";
      Hashtbl.clear externUses; List.map scanDecl l;
      let l = List.map inlineDecl l in
      let l = List.fold_right optimiseDecl l [] in
      if !showOptimisedInstr then Ccode.formatDecls l;
      if !renameLocals then List.map renameDecl l else l
    end
  else
    if !renameLocals then List.map renameDecl l else l
