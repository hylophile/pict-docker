(*
 * Compiles core Pict into C.
 *)

open Ccode

let profiling =
  Flags.createFlag "profiling"
  "*Include extra code for profiling" false

let prof s =
  if !profiling then PROFILING(s) else NULL

let profx s x =
  if !profiling then PROFILING(Printf.sprintf s x) else NULL

let fair =
  Flags.createFlag "fair" "*Use a fair process scheduling strategy" true

let showInstr = Flags.createFlag "showInstr" "*Show C instructions" false

(*
 * User-configureable heap size (compiled into the main program as a constant).
 *)

let heapSize = ref 256

let getSize fi = function
  [size] ->
    (try heapSize := int_of_string size with Failure _ ->
     Error.errAt fi "setHeapSize expected a single integer argument")
| _ ->
    Error.errAt fi "setHeapSize expected a single integer argument"

let () = Flags.createCmd "setHeapSize"
  "Set heap size to n K words (default 256K)" getSize

(*
 * All top-level declarations are stored in this reference cell.
 *)

let decls = ref ([] : decl list)
let storeDecl d = decls := d :: !decls

(*
 * Calculate the maximum number of words of heap used by a process
 *)

let rec heapProc accum = function
  Inter.PRL(p,q) -> heapProc (heapProc accum p) q
| Inter.VAL(l,p) -> heapProc (List.fold_left heapVal accum l) p
| Inter.CCODE(_,ci,_,_,p) -> heapProc (accum + ci.alloc) p
| Inter.CCALL(_,ci,_,_,p) -> heapProc (accum + ci.alloc) p
| Inter.ATOM(_,_,p) -> heapProc accum p
| Inter.IF(_,p,q) -> max (heapProc accum p) (heapProc accum q)
| Inter.NEW(_,_,p) -> heapProc (accum + 2) p
| Inter.INPUT(_,_,_) -> accum + 9
| Inter.OUTPUT(_,_,_) -> accum + 9
| Inter.SEND(_,l) -> accum + 1 + List.length l
| Inter.STRUCT(_) -> Error.bug "Codegen.heapProc"
| Inter.SKIP -> accum

and heapVal accum = function
  Inter.TUPLE(_,l) -> accum + 1 + List.length l
| Inter.DEF(_) | Inter.STRING(_) -> Error.bug "Codegen.heapDef"

(*
 * These tag values MUST agree with those in Runtime/pict.tmpl!!
 *)

let emptyTag = INT 0
let oneReaderTag = INT 1
let oneWriterTag = INT 2
let tupleTag x = INT(x*8+7)
let status x = OFFSET(true,x,0)
let sizeOf x = Ccode.sizeOf (OFFSET(false,x,0))
let value x = OFFSET(true,x,1)
let (%%) c d = SEQ(c,d)

type global = UNKNOWN | CLEAN of Var.var | DIRTY of Var.var * int

type env = {
  mutable last : bool;
  locals : (var * ty) list ref;
  mutable endq : global; mutable free : global
}

let allocFp env x =
  match env.free with
    UNKNOWN ->
      let fp = Var.local() in
      env.locals := (fp,POINTER) :: !(env.locals);
      if x <> 0 then env.free <- DIRTY(fp,x) else env.free <- CLEAN(fp);
      (ASSIGN(VAR fp,Ccode.free), fp, 0)
  | CLEAN(fp) ->
      if x <> 0 then env.free <- DIRTY(fp,x); (NULL, fp, 0)
  | DIRTY(fp,i) ->
      env.free <- DIRTY(fp,i+x); (NULL, fp, i)

let allocEndq env x =
  match env.endq with
    UNKNOWN ->
      let endq = Var.local() in
      env.locals := (endq,POINTER) :: !(env.locals);
      if x <> 0 then env.endq <- DIRTY(endq,x) else env.endq <- CLEAN(endq);
      (ASSIGN(VAR endq,Ccode.endq), endq, 0)
  | CLEAN(endq) ->
      if x <> 0 then env.endq <- DIRTY(endq,x); (NULL, endq, 0)
  | DIRTY(endq,i) ->
      env.endq <- DIRTY(endq,i+x); (NULL, endq, i)

let flushFp env =
  match env.free with
    UNKNOWN -> NULL
  | CLEAN _ -> env.free <- UNKNOWN; NULL
  | DIRTY(fp,i) -> env.free <- UNKNOWN; ASSIGN(Ccode.free,INDEX(fp,INT i))

let flushEndq env =
  match env.endq with
    UNKNOWN -> NULL
  | CLEAN _ -> env.endq <- UNKNOWN; NULL
  | DIRTY(endq,i) -> env.endq <- UNKNOWN; ASSIGN(Ccode.endq,INDEX(endq,INT i))

let flushIfLast env =
  if env.last then (flushFp env %% flushEndq env) else NULL

let subEnv env =
  {last = true; locals = env.locals; endq = env.endq; free = env.free}

(*
 * Allocate a local variable for 'name' and initialise it to
 * the appropriate address (offset from the free pointer).
 *)

let initDef env fp (i,c) = function
  Inter.TUPLE(x,[]) ->
    env.locals := (x,INTEGER) :: !(env.locals);
    (i,ASSIGN(VAR x,INT 0) %% c)
| Inter.TUPLE(x,l) ->
    env.locals := (x,INTEGER) :: !(env.locals);
    (i + List.length l + 1,ASSIGN(VAR x,TAG(fp,i)) %% c)
| Inter.DEF(_) | Inter.STRING(_) -> Error.bug "Codegen.initDef"

let rec transProc env = function
  Inter.PRL(p,q) ->
    let last = env.last in
    let c = transProc (env.last <- false; env) p in
    c %% transProc (env.last <- last; env) q
| Inter.VAL(l,p) ->
    let (load,fp,i) = allocFp env 0 in
    let (_,c) = List.fold_left (initDef env fp) (i,NULL) l in
    let code = List.fold_left (transDef env) NULL l in
    load %% c %% code %% transProc env p
| Inter.CCODE(None,ci,sl,l,p) ->
    if ci.alloc > 0 then
      let store = flushFp env in
      profx "cc.ccode++; cc.ccodeAlloc += %d;\n" ci.alloc %%
      store %% CODE(ci,sl,List.map transAtom l) %% transProc env p
    else
      (CODE(ci,sl,List.map transAtom l) %% transProc env p)
| Inter.CCODE(Some x,ci,sl,l,p) ->
    if ci.alloc > 0 then
      let store = flushFp env in
      env.locals := (x,INTEGER) :: !(env.locals);
      profx "cc.ccode++; cc.ccodeAlloc += %d;\n" ci.alloc %%
      store %% ASSIGN(VAR x,CCODE(ci,sl,List.map transAtom l)) %%
      transProc env p
    else
      (env.locals := (x,INTEGER) :: !(env.locals);
       ASSIGN(VAR x,CCODE(ci,sl,List.map transAtom l)) %% transProc env p)
| Inter.CCALL(None,ci,s,l,p) ->
    if ci.alloc > 0 then
      let store = flushFp env in
      profx "cc.ccode++; cc.ccodeAlloc += %d;\n" ci.alloc %%
      store %% CALL(ci,s,List.map transAtom l) %% transProc env p
    else
      (CALL(ci,s,List.map transAtom l) %% transProc env p)
| Inter.CCALL(Some x,ci,s,l,p) ->
    if ci.alloc > 0 then
      let store = flushFp env in
      env.locals := (x,INTEGER) :: !(env.locals);
      profx "cc.ccode++; cc.ccodeAlloc += %d;\n" ci.alloc %%
      store %% ASSIGN(VAR x,CCALL(ci,s,List.map transAtom l)) %%
      transProc env p
    else
      (env.locals := (x,INTEGER) :: !(env.locals);
       ASSIGN(VAR x,CCALL(ci,s,List.map transAtom l)) %% transProc env p)
| Inter.ATOM(x,a,p) ->
    env.locals := (x,INTEGER) :: !(env.locals);
    ASSIGN(VAR x,transAtom a) %% transProc env p
| Inter.IF(a,p1,p2) ->
    let env1 = subEnv env and env2 = subEnv env in
    let c1 = transProc env1 p1 and c2 = transProc env2 p2 in
    (env.endq <- UNKNOWN; env.free <- UNKNOWN; IF(transAtom a,c1,c2))
| Inter.NEW(x,init,p) ->
    let (load,fp,i) = allocFp env 2 in
    env.locals := (x,INTEGER) :: !(env.locals);
    prof "cc.new++; cc.chanAlloc += 2;\n" %%
    load %% ASSIGN(VAR x,TAG(fp,i)) %%
    (match init with Inter.NOTREQUIRED -> NULL |
     Inter.REQUIRED -> ASSIGN(DEREF(fp,i),emptyTag)) %%
    transProc env p
| Inter.INPUT(x,y,Inter.EMPTY) ->
    prof "cc.INPUTk++; cc.INPUTe++;\n" %%
    ASSIGN(status(transAtom x),oneReaderTag) %%
    ASSIGN(value(transAtom x),transAtom y) %%
    flushIfLast env
| Inter.INPUT(x,y,_) ->
    flushFp env %% flushEndq env %%
    Ccode.complexInput (transAtom x) (transAtom y)
| Inter.OUTPUT(x,y,Inter.EMPTY) ->
    prof "cc.OUTPUTk++; cc.OUTPUTe++;\n" %%
    ASSIGN(status(transAtom x),oneWriterTag) %%
    ASSIGN(value(transAtom x),transAtom y) %%
    flushIfLast env
| Inter.OUTPUT(x,y,Inter.ONEREADER) ->
    let (load,endq,i) = allocEndq env (-2) in
    prof "cc.OUTPUTk++; cc.OUTPUT1r++; cc.runqAlloc += 2;\n" %%
    load %% ASSIGN(status(transAtom x),emptyTag) %%
    ASSIGN(DEREF(endq,i),value(transAtom x)) %%
    ASSIGN(DEREF(endq,i-1),transAtom y) %%
    flushIfLast env 
| Inter.OUTPUT(x,y,Inter.REPLICATED) ->
    let (load,endq,i) = allocEndq env (-2) in
    prof "cc.OUTPUTk++; cc.OUTPUTdef++; cc.runqAlloc += 2;\n" %%
    load %% ASSIGN(DEREF(endq,i),transAtom x) %%
    ASSIGN(DEREF(endq,i-1),transAtom y) %%
    flushIfLast env
| Inter.OUTPUT(x,y,_) ->
    flushFp env %% flushEndq env %%
    Ccode.complexOutput (transAtom x) (transAtom y)
| Inter.SEND(x,l) ->
    let len = List.length l + 1 in
    let (load,endq,i) = allocEndq env (- len) in
    let writeArg j a c = ASSIGN(DEREF(endq,i-j-1),transAtom a) %% c in
    profx "cc.OUTPUTk++; cc.OUTPUTdef++; cc.runqAlloc += %d;\n" len %%
    load %% ASSIGN(DEREF(endq,i),transAtom x) %%
    Misc.itFold writeArg NULL l %% flushIfLast env
| Inter.SKIP ->
    flushIfLast env
| Inter.STRUCT(_) ->
    Error.bug "Codegen.transProc"

and transAtom = function
  Inter.CHAR(c) -> let i = Char.code c in INT(i+i)
| Inter.PROJECT(a,i) -> OFFSET(false,transAtom a,1+i)
| Inter.ADDR(x) -> ADDR x
| Inter.STATIC(x) -> TAGS x
| Inter.DYNAMIC(x) -> VAR x
| Inter.BOOL(true) -> INT 1
| Inter.BOOL(false) -> INT 0
| Inter.INT(i) -> INT(i+i)
| Inter.COERCION(c,a) -> COERCION(c,transAtom a)
| Inter.CCONST(ci,s) -> CCODE(ci,[s],[])

and transDef env c = function
  Inter.TUPLE(x,[]) -> c
| Inter.TUPLE(x,l) ->
    let sz = List.length l + 1 in
    let (load,fp,i) = allocFp env sz in
    let initField j a c = c %% ASSIGN(DEREF(fp,i+j+1),transAtom a) in
    profx "cc.tuple++; cc.tupleAlloc += %d;\n" sz %%
    c %% load %% ASSIGN(DEREF(fp,i),tupleTag sz) %%
    Misc.itFold initField NULL l
| Inter.DEF(_) | Inter.STRING(_) ->
    Error.bug "Codegen.transDef: illegal value"

let transConst = function
  Inter.DEF(x,_,args,p) ->
    (*
     * Work out the maximum amount of space required by the process.
     *)
    let words = heapProc 0 p in
    (*
     * Initial environment.
     *)
    let free = Var.local() in let endq = Var.local() in
    let addArg l o = match o with None -> l | Some x -> (x,INTEGER) :: l in
    let locals = List.fold_left addArg [] args in
    let env = {
      last = true; free = CLEAN(free); endq = CLEAN(endq);
      locals = ref ((free,POINTER) :: (endq,POINTER) :: locals)
    } in
    (*
     * Build the code.
     *)
    let code =
      if !fair then
	begin
	  let l = List.length args and s = Var.local() in
	  let loadArg i o c =
	    match o with None -> c | Some x -> ASSIGN(VAR x,DEREF(s,-i)) %% c
          in
	    env.locals := (s,POINTER) :: !(env.locals);
	    ASSIGN(VAR s,Ccode.startq) %%
	    Misc.itFold loadArg NULL args %%
	    ASSIGN(Ccode.startq,INDEX(s,INT(-l))) %%
	    transProc env p
	end
      else
	begin
	  let l = List.length args in
	  let (load,endq,i) = allocEndq env l in
	  let loadArg i o c =
	    match o with None -> c | Some x ->
	    ASSIGN(VAR x,DEREF(endq,i+l-i)) %% c
	  in
	    Misc.itFold loadArg NULL args %%
	    transProc env p
	end
    in
    (*
     * The code for the closure.
     *)
    storeDecl(BLOCK(x,!(env.locals),free,endq,words,code))
| Inter.TUPLE(x,l) ->
    let tag = tupleTag(List.length l+1) in
    storeDecl(CONST(x,(tag :: List.map transAtom l)))
| Inter.STRING(_,_) ->
    ()

let initConst extern = function
  Inter.DEF(x,_,_,_) -> storeDecl(FORWARD x)
| Inter.TUPLE(x,_) -> storeDecl(EXTERN x)
| Inter.STRING(x,s) ->
    if extern then storeDecl(EXTERN x) else storeDecl(BYTES(x,s))

let initToplevel s = storeDecl(TOPLEVEL s)

let initImport prog =
  List.iter initToplevel prog.Inter.includes;
  List.iter (initConst true) prog.Inter.constants

let stdIncludes = 
  "#include \"pict.h\"\n#include <stdio.h>\n#include <stdlib.h>\n" ^
  "#include <string.h>\n#include <unistd.h>\n"

let trans imports prog =
  storeDecl(TOPLEVEL stdIncludes);
  if !fair then
    storeDecl(TOPLEVEL(
      "void (*scheduler) (void) = &fair;"
    ))
  else
    storeDecl(TOPLEVEL(
      "void (*scheduler) (void) = &unfair;"
    ));
  (* Set the heap size *)
  storeDecl(TOPLEVEL(
    "int heapSizeInWords = " ^ string_of_int(!heapSize*1024) ^ ";"
  ));
  (* Generate extern declarations for imported identifiers *)
  List.iter initImport imports;
  (* Generate headers and toplevel code for constants *)
  List.iter initToplevel prog.Inter.includes;
  List.iter initToplevel prog.Inter.static;
  List.iter (initConst false) prog.Inter.constants;
  (* Generate code for constants *)
  List.iter transConst prog.Inter.constants;
  (* Translate the top-level program *)
  transConst (Inter.DEF(Var.main,Inter.NOTINLINABLE,[],prog.Inter.proc));
  let l = List.rev (!decls) in
  if !showInstr then formatDecls l;
  decls := []; (Copt.optimise l)

let constants imports prog =
  storeDecl(TOPLEVEL stdIncludes);
  (* Generate extern declarations for imported identifiers *)
  List.iter initImport imports;
  (* Generate headers and toplevel code for constants *)
  List.iter initToplevel prog.Inter.includes;
  List.iter initToplevel prog.Inter.static;
  List.iter (initConst false) prog.Inter.constants;
  (* Translate the constants *)
  List.iter transConst prog.Inter.constants;
  let l = List.rev (!decls) in
  if !showInstr then formatDecls l;
  decls := []; (Copt.optimise l)
