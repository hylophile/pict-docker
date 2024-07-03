(*
 * Work out occurence information.
 *)

open Inter

let assumeAllImpure =
  Flags.createFlag "assumeAllImpure"
  "*Assume that all C code is impure (for optimiser debugging)" false

let initEnv = ([],0)
let newLevel u (l,level) = (u :: l,level+1)

let levelTable = (Hashtbl.create 2000 : (Var.var,int) Hashtbl.t)
let projTable = (Hashtbl.create 2000 : (Var.var,int ref) Hashtbl.t)
let operTable = (Hashtbl.create 2000 : (Var.var,int ref) Hashtbl.t)
let argTable = (Hashtbl.create 2000 : (Var.var,int ref) Hashtbl.t)

let calcUsage (l,level) x =
  let rec accum u level l =
    if level = 0 then u else
    match l with
      [] -> Error.bug "Occur.calcUsage"
    | i :: rest -> accum (u*i) (level-1) rest
in
  accum 1 (level - Hashtbl.find levelTable x) l

let used table env x =
  try let r = Hashtbl.find table x in r := !r + (calcUsage env x)
  with Not_found -> ()

let addBound (_,level) x =
  Hashtbl.add levelTable x level; Hashtbl.add operTable x (ref 0);
  Hashtbl.add projTable x (ref 0); Hashtbl.add argTable x (ref 0)

let addBinder env = function None -> () | Some x -> addBound env x

let proj x = try !(Hashtbl.find projTable x) with Not_found -> 999
let oper x = try !(Hashtbl.find operTable x) with Not_found -> 999
let arg x = try !(Hashtbl.find argTable x) with Not_found -> 999

let rec projAtom env = function
  ADDR x -> used projTable env x
| STATIC x -> used projTable env x
| DYNAMIC x -> used projTable env x
| PROJECT(t,_) -> projAtom env t
| COERCION(_,a) -> operAtom env a
| CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> ()

and operAtom env = function
  ADDR x -> used operTable env x
| STATIC x -> used operTable env x
| DYNAMIC x -> used operTable env x
| PROJECT(t,_) -> projAtom env t
| COERCION(_,a) -> operAtom env a
| CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> ()

and argAtom env = function
  ADDR x -> used argTable env x
| STATIC x -> used argTable env x
| DYNAMIC x -> used argTable env x
| PROJECT(t,_) -> projAtom env t
| COERCION(_,a) -> argAtom env a
| CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> ()

let scanVal env = function
  TUPLE(x,_) -> addBound env x
| DEF(x,_,_,_) -> addBound env x
| STRING(x,_) -> addBound env x

let rec occVal env (changed,unused) v =
  match v with
    TUPLE(x,l) ->
      (match oper x + arg x + proj x with
         0 -> (changed,v :: unused)
       | u -> List.iter (argAtom (newLevel u env)) l; (true,unused))
  | DEF(x,INLINABLE,args,p) ->
      (match oper x + arg x with
         0 -> (changed,v :: unused)
       | u -> occAbs env u args p; (true,unused))
  | DEF(x,NOTINLINABLE,args,p) ->
      (match oper x + arg x with
         0 -> (changed,v :: unused)
       | _ -> occAbs env 1 args p; (true,unused))
  | STRING(x,_) ->
      (match oper x + arg x with
         0 -> (changed,v :: unused)
       | _ -> (true,unused))

and occAbs env u args p =
  let env = newLevel u env in List.iter (addBinder env) args; occProc env p

and fixpoint env unused =
  let (changed,unused) = List.fold_left (occVal env) (false,[]) unused in
  if changed then fixpoint env unused else ()

(*
 * [occProc p] optimises the process [p] in parallel with
 * the input atoms [inp], output atoms [out] and other processes [rest].
 *)
and occProc env = function
  PRL(p,q) -> occProc env p; occProc env q
(*
 * In the case of a mutually recursive definition, we first scan over all
 * the definitions, recording the names they bind. This ensures that when
 * we optimise [p] we know that all the names bound in [l] are bound to
 * process definitions. The list [ul] is just the list [l] where each
 * element is tagged with a boolean reference cell. If [p] uses any of
 * the definitions in [ul] then the appropriate boolean will be set to true.
 * Using this information, the [optDefs] function works out the minimal
 * list of definitions which are required by [p] (if this list is empty,
 * we eliminate the whole definition).
 *)
| VAL(l,p) ->
    List.iter (scanVal env) l; occProc env p; fixpoint env l
| CCODE(None,ci,_,l,p) ->
    List.iter (argAtom env) l; occProc env p
| CCODE(Some x,ci,_,l,p) ->
    addBound env x; occProc env p;
    (match (oper x + arg x + proj x,!assumeAllImpure || ci.Ccode.writer) with
     (0,false) -> () | (u,_) -> List.iter (argAtom (newLevel u env)) l)
| CCALL(None,ci,_,l,p) ->
    List.iter (argAtom env) l; occProc env p
| CCALL(Some x,ci,_,l,p) ->
    addBound env x; occProc env p;
    (match (oper x + arg x + proj x,!assumeAllImpure || ci.Ccode.writer) with
     (0,false) -> () | (u,_) -> List.iter (argAtom (newLevel u env)) l)
| ATOM(x,a,p) ->
    addBound env x; occProc env p;
    (match oper x + arg x + proj x with 0 -> () |
     u -> argAtom (newLevel u env) a)
| STRUCT(inc) ->
    let env = newLevel 2 env in
    Types.allBindings (fun (_,a) -> argAtom env a) inc
| IF(a,p,q) -> argAtom env a; occProc env p; occProc env q
| NEW(x,i,p) -> addBound env x; occProc env p
| OUTPUT(x,y,s) -> operAtom env x; argAtom env y
| INPUT(x,y,s) -> operAtom env x; argAtom env y
| SEND(x,l) -> operAtom env x; List.iter (argAtom env) l
| SKIP -> ()

let occur _ prog =
  Hashtbl.clear operTable; Hashtbl.clear projTable; Hashtbl.clear argTable;
  List.iter (scanVal initEnv) prog.constants; occProc initEnv prog.proc;
  fixpoint initEnv prog.constants
