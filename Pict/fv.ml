(*
 * Calculates free variable information.
 *
 * Possible improvements:
 *
 * 1) Omit closure argument in case where all call points are known and
 *    there are no free variables.
 * 2) Omit function pointer from closure when call points are known.
 * 3) Don't put 'self' into closure for recursive def.
 *)

open Inter

let possibleConst = function
  CCONST(ci,_) -> ci.Ccode.const
| DYNAMIC(_) | ADDR(_) | STATIC(_) | CHAR(_) | BOOL(_) | INT(_) -> true
| COERCION(_,_) | PROJECT(_,_) -> false

let bindingLevel = Hashtbl.create 500
let candidateGlobals = Hashtbl.create 500
let nonGlobals = Hashtbl.create 500
let changed = ref true
let constants = ref []

let initEnv = (0,[])

let pushEnv (level,refs) n =
  let i = (ref false,ref []) in Hashtbl.add candidateGlobals n i;
  (succ level,i :: refs)

let addBound (level,_) x = Hashtbl.add bindingLevel x (level,ref 0)

let addBinder env = function None -> () | Some x -> addBound env x

let rec addEntry reqd x = function
  [] -> if reqd then [(x,1)] else [(x,0)]
| ((y,u) as occ :: ys) ->
  if x = y then if reqd then (y,u+1) :: ys else (y,u) :: ys
  else occ :: addEntry reqd x ys

let rec addFv reqd x i l =
  if i = 0 then () else
  match l with
    [] -> Error.bug "fv.addFv"
  | ((_,r) :: refs) -> r := addEntry reqd x !r; addFv reqd x (pred i) refs

let scanVar (level,refs) x =
  try
    let (l,r) = Hashtbl.find bindingLevel x in
    incr r; addFv true x (level - l) refs
  with Not_found -> ()
    (* Variables without a binding level are constants *)

let addStruct (_,refs) = List.iter (fun (s,_) -> s := true) refs

let rec scanProc env = function
  PRL(p,q) -> scanProc env p; scanProc env q
| NEW(x,_,p) -> addBound env x; scanProc env p
| VAL(l,p) ->
    List.iter (scanRec env) l; List.iter (scanVal env) l; scanProc env p
| CCODE(None,_,_,l,p) -> List.iter (scanAtom env) l; scanProc env p
| CCODE(Some x,_,_,l,p) ->
    List.iter (scanAtom env) l; addBound env x; scanProc env p
| CCALL(None,_,_,l,p) -> List.iter (scanAtom env) l; scanProc env p
| CCALL(Some x,_,_,l,p) ->
    List.iter (scanAtom env) l; addBound env x; scanProc env p
| ATOM(x,a,p) -> scanAtom env a; addBound env x; scanProc env p
| IF(a,p,q) -> scanAtom env a; scanProc env p; scanProc env q
| INPUT(x,y,_) -> scanAtom env x; scanAtom env y
| OUTPUT(x,y,_) -> scanAtom env x; scanAtom env y
| SEND(x,l) -> scanAtom env x; List.iter (scanAtom env) l
| STRUCT(c) ->
    addStruct env; Types.allBindings (fun (_,a) -> scanAtom env a) c
| SKIP -> ()

and scanRec env = function
  TUPLE(x,_) -> addBound env x
| DEF(x,_,_,_) -> addBound env x
| STRING(x,_) -> addBound env x

and scanImport = function
  PRL(p,q) -> scanImport p; scanImport q
| VAL(l,p) -> List.iter scanImportVal l; scanImport p
| CCODE(None,_,_,_,p) -> scanImport p
| CCODE(Some x,_,_,_,p) -> addBound initEnv x; scanImport p
| CCALL(None,_,_,_,p) -> scanImport p
| CCALL(Some x,_,_,_,p) -> addBound initEnv x; scanImport p
| ATOM(x,_,p) -> addBound initEnv x; scanImport p
| IF(_,p,q) -> scanImport p; scanImport q
| NEW(x,_,p) -> addBound initEnv x; scanImport p
| INPUT(_) | OUTPUT(_) | SEND(_) | STRUCT(_) | SKIP -> ()

and scanImportVal = function
  TUPLE(x,_) -> addBound initEnv x
| DEF(x,_,args,p) ->
    addBound initEnv x; List.iter (addBinder initEnv) args; scanImport p
| STRING(x,_) -> addBound initEnv x

and scanTuple env vars = function
  [] -> vars
| DYNAMIC(x) :: l -> scanVar env x; scanTuple env ((x,0) :: vars) l
| _ :: l -> scanTuple env vars l

and scanVal env = function
  TUPLE(x,l) ->
    if List.for_all possibleConst l then
      Hashtbl.add candidateGlobals x (ref false,ref(scanTuple env [] l))
    else
      List.iter (scanAtom env) l
| DEF(x,_,args,p) ->
    let newEnv = pushEnv env x in
    List.iter (addBinder newEnv) args; scanProc newEnv p
| STRING(x,_) -> Hashtbl.add candidateGlobals x (ref false,ref [])

and scanAtom env = function
  DYNAMIC x -> scanVar env x
| PROJECT(a,_) -> scanAtom env a
| COERCION(_,a) -> scanAtom env a
| ADDR(_) | STATIC(_) | CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> ()

let notCandidate (x,_) =
  try Hashtbl.find candidateGlobals x; false with Not_found -> true

let findNonGlobals x ((s,refs) as i) =
  if !s || List.exists notCandidate !refs then
    (Hashtbl.remove candidateGlobals x;
     Hashtbl.add nonGlobals x i;
     changed := true)
  else
    ()

let rec removeGlobals l = function
  [] -> l
| ((x,_) as occ :: xs) ->
  try Hashtbl.find candidateGlobals x; removeGlobals l xs
  with Not_found -> removeGlobals (occ :: l) xs

let fv n =
  try Hashtbl.find candidateGlobals n; (false,[]) with Not_found ->
  let (s,r) = Hashtbl.find nonGlobals n in (!s,removeGlobals [] !r)

let rec fvProc = function
  PRL(p,q) -> PRL(fvProc p,fvProc q)
| VAL(l,p) ->
  (match List.fold_left fvDef [] l with
   [] -> fvProc p | l -> VAL(l,fvProc p))
| CCODE(x,ci,sl,l,p) -> CCODE(x,ci,sl,List.map fvAtom l,fvProc p)
| CCALL(x,ci,s,l,p) -> CCALL(x,ci,s,List.map fvAtom l,fvProc p)
| ATOM(x,a,p) -> ATOM(x,fvAtom a,fvProc p)
| IF(a,p,q) -> IF(fvAtom a,fvProc p,fvProc q)
| NEW(x,i,p) -> NEW(x,i,fvProc p)
| INPUT(x,y,s) -> INPUT(fvAtom x,fvAtom y,s)
| OUTPUT(x,y,s) -> OUTPUT(fvAtom x,fvAtom y,s)
| SEND(x,l) -> SEND(fvAtom x,List.map fvAtom l)
| STRUCT(inc) -> STRUCT(Types.mapBindings (fun (t,x) -> (t,fvAtom x)) inc)
| SKIP -> SKIP

and fvAtom a =
  match a with
    ADDR(_) | STATIC(_) | CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> a
  | PROJECT(a,i) -> PROJECT(fvAtom a,i)
  | COERCION(c,a) -> COERCION(c,fvAtom a)
  | DYNAMIC x ->
    try Hashtbl.find candidateGlobals x; STATIC(x) with Not_found -> a

and fvDef defs = function
  TUPLE(x,l) ->
    let t = TUPLE(x,List.map fvAtom l) in
    (try
       match fv x with
	 (_,[]) -> constants := t :: !constants; defs
       | (_,_) -> t :: defs
     with Not_found -> t :: defs)
| DEF(n,i,l,p) ->
    (match fv n with
       (false,[]) ->
	 let f = Var.fresh() in
         let def = DEF(f,i,None :: l,fvProc p) in
	 constants := def :: TUPLE(n,[ADDR f]) :: !constants;
	 defs
     | (false,vars) ->
	 let f = Var.fresh() in let cl = Var.fresh() in
         let selectFv i (x,_) p = ATOM(x,PROJECT(DYNAMIC cl,i+1),p) in
	 let p = Misc.itFold selectFv (fvProc p) vars in
         constants := DEF(f,i,Some cl :: l,p) :: !constants;
	 let vars = List.map (fun (x,_) -> DYNAMIC x) vars in
	 TUPLE(n,ADDR f :: vars) :: defs
     | (true,_) ->
         DEF(n,i,l,fvProc p) :: defs)
| STRING(_) as v -> constants := v :: !constants; defs

let fv imports prog =
  constants := prog.constants; changed := true;
  List.iter (fun prog -> scanImport prog.proc) imports;
  scanProc initEnv prog.proc;
  while !changed do
    changed := false; Hashtbl.iter findNonGlobals candidateGlobals
  done;
  let p = fvProc prog.proc in let l = !constants in
  constants := []; Hashtbl.clear bindingLevel;
  Hashtbl.clear candidateGlobals; Hashtbl.clear nonGlobals;
  {includes = prog.includes; static = prog.static; constants = l; proc = p}
