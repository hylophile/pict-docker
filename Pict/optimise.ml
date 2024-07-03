(*
 * Various intermediate code optimisations.
 *)

(*
 * TODO: remove deadlocked inputs and outputs.
 * Need to do a better job of optimising (and generating code for)
 * nested conditionals (cf. stringBool, cmp.pr).  Then int.max and int.abs
 * could be done as simple conditionals.
 * Do automatic detection of inlinable process defs (then remove INLINABLE
 * hint from continuations and lib code, and duplicate i/o atoms across
 * conditionals.)  Probably add inverse 'noinline' pragma.
 * Fix prim.alloc memory usage when string size is known.
 * Lift out strings to top level, combining duplicates (nice for "").
 * Shortcut (val b = (not x) if b then p else q) to if b then q else p
 *)

open Inter

let optimise = Flags.createFlag "optimise" "Optimise intermediate code" true
let optimiseCCalls = Flags.createFlag "optimiseCCalls" "*Optimise C calls" true
let optCom = Flags.createFlag "optCom" "*Optimise communications" true

let fixpointOptimise = Flags.createFlag "fixpointOptimise" 
  "*Optimise intermediate code until we reach a fixpoint" false

let lint = Flags.createFlag "lint" 
  "*Run lint (to check consistency of intermediate code)" true

let showCodeStats = Flags.createFlag "showCodeStats" 
  "*Show statistics for intermediate code" false

let showImports = Flags.createFlag "showImports" 
  "*Show intermediate code imported from other modules" false

let showInter = Flags.createFlag "showInter" 
  "*Show intermediate code during optimisation" false

let showOptimisedInter = Flags.createFlag "showOptimisedInter" 
  "*Show fully optimised intermediate code" false

let showOccur =
  Flags.createFlag "showOccur" "*Show occurrence information" false

let printOccur x =
  if !showOccur then
    (Format.print_string "{"; Format.print_int (Occur.oper x);
     Format.print_string ","; Format.print_int (Occur.proj x);
     Format.print_string ","; Format.print_int (Occur.arg x);
     Format.print_string "}")
  else
    ()

type env =
  {inp : (atom * atom) list; out : (atom * atom) list; rest : proc list}

let initEnv = {inp = []; out = []; rest = []}
let emptyEnv env = {inp = []; out = []; rest = []}
let addRest env p = {inp = env.inp; out = env.out; rest = p :: env.rest}

(*
 * The variable [improved] is set to [false] at the start of each optimisation
 * pass.  It is set to [true] whenever we make a (significant) improvement
 * to the program, so that we know that it (probably) worthwhile to run the
 * optimiser again.
 *)
let improved = ref false

(*
 * [atomTable] maps uniquely-bound variables to atoms.
 * [tupleTable] maps uniquely-bound variables to tuples of atoms.
 * [defTable] maps uniquely-bound variables to process abstractions.
 *)
let atomTable = (Hashtbl.create 2000 : (Var.var,atom) Hashtbl.t)
let tupleTable = (Hashtbl.create 2000 : (Var.var,atom list) Hashtbl.t)
let defTable =
  (Hashtbl.create 2000 : (Var.var,bool * Var.var option list * proc) Hashtbl.t)

(*
 * [addAtom x a] adds a mapping from [x] to [a] to [atomTable].
 *)
let addAtom x a = Hashtbl.add atomTable x a

let defInfo x =
  try Some(Hashtbl.find defTable x) with Not_found -> None

let tupleInfo x =
  try Some(Hashtbl.find tupleTable x) with Not_found -> None

(*
 * [optAtom a] optimises [a]. We only set [improved] to [true] when we
 * make a substantial improvement (ie. when we managed to eliminate a
 * PROJECT operation).
 *)
let rec optAtom a = match a with
  ADDR(_) | CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> a
| STATIC(x) -> (try Hashtbl.find atomTable x with Not_found -> a)
| DYNAMIC(x) -> (try Hashtbl.find atomTable x with Not_found -> a)
| COERCION(c1,COERCION(c2,a)) ->
    (match (c1,c2) with
       (Ccode.INT2C,Ccode.C2INT) -> improved := true; optAtom a
     | (Ccode.BOOL2C,Ccode.C2BOOL) -> improved := true; optAtom a
     | (_,_) -> COERCION(c1,COERCION(c2,optAtom a)))
| COERCION(c,a) -> COERCION(c,optAtom a)
| PROJECT(t,i) ->
    let t = optAtom t in
    (match t with
       STATIC x ->
       (match tupleInfo x with Some(l) -> improved := true; List.nth l i
	| None -> Error.bug "optAtom: PROJECT from STATIC")
     | DYNAMIC x ->
       (match tupleInfo x with Some(l) -> improved := true; List.nth l i
	| None -> PROJECT(t,i))
     | _ -> PROJECT(t,i))

let stringTable = (Hashtbl.create 100 : (string,bool * Var.var) Hashtbl.t)

let storeString extern x s =
  try snd(Hashtbl.find stringTable s) with Not_found ->
  (Hashtbl.add stringTable s (extern,x); x)

let addStrings l =
  let constants = ref l in
  let add s (e,x) = if e then () else constants := STRING(x,s) :: !constants in
  Hashtbl.iter add stringTable;
  !constants

let rec mustFail = function
  PRL(p,q) -> mustFail p && mustFail q
| VAL(_,p) -> mustFail p
| CCODE(None,_,sl,_,p) ->
  (match sl with ["exit(I("; "));"] -> true | _ -> mustFail p)
| CCODE(Some _,_,_,_,p) -> mustFail p
| CCALL(None,_,s,_,p) -> (match s with "exit" -> true | _ -> mustFail p)
| CCALL(Some _,_,_,_,p) -> mustFail p
| ATOM(_,_,p) -> mustFail p
| IF(_,p,q) -> mustFail p && mustFail q
| NEW(_,_,p) -> mustFail p
| STRUCT(_) | SKIP | OUTPUT(_) | INPUT(_) | SEND(_) -> false

let rec findVar x = function
  [] -> None
| (DYNAMIC y,a) as p :: l ->
    if x = y then Some(a,l) else
    (match findVar x l with None -> None | Some(a,l) -> Some(a,p :: l))
| (PROJECT _,_) as p :: l ->
    (match findVar x l with None -> None | Some(a,l) -> Some(a,p :: l))
| (COERCION _,_) :: _ | (STATIC _,_) :: _ | (ADDR _,_) :: _ | (CHAR _,_) :: _
| (BOOL _,_) :: _ | (INT _,_) :: _ | (CCONST _,_) :: _ -> 
    Error.bug "Optimise.findVar"

let findMatch a l =
  match a with
    DYNAMIC x ->
    (try Hashtbl.find defTable x; Some(a,l) with Not_found -> findVar x l)
  | STATIC x -> Some(a,l)
  | PROJECT(_,_) -> None
  | COERCION(_) | ADDR(_) | CHAR(_) | BOOL(_) | INT(_) | CCONST(_) -> 
      Error.bug "Optimise.findMatch"

let scanConst = function
  TUPLE(x,l) -> Hashtbl.add tupleTable x l
| DEF(x,INLINABLE,args,p) -> Hashtbl.add defTable x (true,args,p)
| DEF(x,NOTINLINABLE,args,p) -> ()
| STRING(x,s) -> storeString true x s; ()

let optTuple rest v =
  match v with
    TUPLE(x,[]) -> Hashtbl.add atomTable x (INT 0); rest
  | TUPLE(x,l) ->
      (match (Occur.oper x,Occur.proj x,Occur.arg x) with
         (0,0,0) -> rest
       | (0,_,0) -> Hashtbl.add tupleTable x (List.map optAtom l); rest
       | (_,_,_) ->
	   let l = List.map optAtom l in
	   Hashtbl.add tupleTable x l; TUPLE(x,l) :: rest)
  | STRING(x,s) ->
      Hashtbl.add atomTable x (STATIC(storeString false x s)); rest
  | DEF(_) -> v :: rest

let rec optInlinableDef rest v =
  match v with
    DEF(x,INLINABLE,args,p) ->
      (match (Occur.oper x,Occur.arg x) with
         (0,0) -> rest
       | (1,0) -> Hashtbl.add defTable x (false,args,p); rest
       | (_,0) -> Hashtbl.add defTable x (true,args,optProc initEnv p); rest
       | (_,_) -> Hashtbl.add defTable x (true,args,p); v :: rest)
  | DEF(x,NOTINLINABLE,args,p) ->
      (match (Occur.oper x,Occur.arg x) with
         (0,0) -> rest
       | (1,0) -> Hashtbl.add defTable x (false,args,p); rest
       | _ -> v :: rest)
  | STRING(_) -> Error.bug "Optimise.optDef"
  | TUPLE(_) -> v :: rest

and optDef rest v =
  match v with
    DEF(x,INLINABLE,args,p) ->
      let p = optProc initEnv p in
      Hashtbl.add defTable x (true,args,p);
      DEF(x,INLINABLE,args,p) :: rest
  | DEF(x,NOTINLINABLE,args,p) ->
      DEF(x,NOTINLINABLE,args,optProc initEnv p) :: rest
  | STRING(_) -> Error.bug "Optimise.optDef"
  | TUPLE(_) -> v :: rest

(*
 * [optProc env p] optimises the process [p] in parallel with
 * the input atoms [inp], output atoms [out] and other processes [rest].
 *)
and optProc env = function
  PRL(p,q) -> optProc (addRest env q) p
(*
 * We eliminate empty sequences of value declarations.
 *)
| VAL([],p) -> optProc env p
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
    let l = List.fold_left optTuple [] l in
    let l = List.fold_left optInlinableDef [] l in
    let l = List.fold_left optDef [] l in
    VAL(l,optProc env p)
(*
 * A piece of CCODE which has no explicit result can never be eliminated,
 * since it always has side-effects.   We do, however, remove code which
 * checks assertions which we know are true, and code which follows an exit).
 *)
| CCALL(b,ci,s,l,SKIP) ->
    CCALL(b,ci,s,List.map optAtom l,finish SKIP env)
| CCALL(b,ci,s,l,p) ->
    if !optimiseCCalls then
      (match s with
        "exit" -> improved := true; CCALL(b,ci,s,List.map optAtom l,finish SKIP env)
       | _ -> CCALL(b,ci,s,List.map optAtom l,optProc env p))
    else
      CCALL(b,ci,s,List.map optAtom l,optProc env p)
| CCODE(None,ci,sl,l,SKIP) ->
    CCODE(None,ci,sl,List.map optAtom l,finish SKIP env)
| CCODE(None,ci,sl,l,p) ->
    if !optimiseCCalls then
      (match sl with
         ["exit(I("; "));"] ->
	   improved := true; CCODE(None,ci,sl,List.map optAtom l,finish SKIP env)
       | _ ->
	   CCODE(None,ci,sl,List.map optAtom l,optProc env p))
    else
      CCODE(None,ci,sl,List.map optAtom l,optProc env p)
| CCODE(Some x,ci,sl,l,p) ->
    if (Occur.oper x + Occur.arg x + Occur.proj x) > 0 then
      if !optimiseCCalls then
	match List.map optAtom l with
	  [INT x1; INT x2] as l ->
	    (match sl with
	       ["("; " + "; ")"] -> optATOM env x (INT(x1 + x2)) p
	     | ["("; " - "; ")"] -> optATOM env x (INT(x1 - x2)) p
	     | ["("; " * I("; "))"] -> optATOM env x (INT(x1 * x2)) p
	     | ["("; " == "; ")"] -> optATOM env x (BOOL(x1 = x2)) p
	     | ["("; " != "; ")"] -> optATOM env x (BOOL(x1 != x2)) p
	     | ["("; " > "; ")"] -> optATOM env x (BOOL(x1 > x2)) p
	     | ["("; " < "; ")"] -> optATOM env x (BOOL(x1 < x2)) p
	     | ["("; " >= "; ")"] -> optATOM env x (BOOL(x1 >= x2)) p
	     | ["("; " <= "; ")"] -> optATOM env x (BOOL(x1 <= x2)) p
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [INT i; a] as l ->
	    (match sl with
	       ["("; " + "; ")"] ->
		 if i = 0 then optATOM env x a p
		 else CCODE(Some x,ci,sl,l,optProc env p)
	     | ["("; " * I("; ")"] -> CCODE(Some x,ci,sl,[a; INT i],optProc env p)
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [a; INT i] as l ->
	    (match sl with
	       ["("; " + "; ")"] ->
		 if i = 0 then optATOM env x a p
		 else CCODE(Some x,ci,sl,l,optProc env p)
	     | ["("; " - "; ")"] ->
		 if i = 0 then optATOM env x a p
		 else CCODE(Some x,ci,sl,l,optProc env p)
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [BOOL b; a] as l ->
	    (match sl with
	       ["("; " & "; ")"] ->
		 if b then optATOM env x a p else optATOM env x (BOOL false) p
	     | ["("; " | "; ")"] ->
		 if b then optATOM env x (BOOL true) p else optATOM env x a p
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [a; BOOL b] as l ->
	    (match sl with
	       ["("; " & "; ")"] ->
		 if b then optATOM env x a p else optATOM env x (BOOL false) p
	     | ["("; " | "; ")"] ->
		 if b then optATOM env x (BOOL true) p else optATOM env x a p
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [BOOL b] as l ->
	    (match sl with
	       ["("; " ^ TT)"] -> optATOM env x (BOOL(not b)) p
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| [a] as l ->
	    (match sl with
	       [""; ""] -> optATOM env x a p
	     | _ -> CCODE(Some x,ci,sl,l,optProc env p))
	| l ->
	    CCODE(Some x,ci,sl,l,optProc env p)
      else
	CCODE(Some x,ci,sl,List.map optAtom l,p)
   else
     optProc env p
(*
 * We record the binding of [x] to [a] in [atomTable], optimise [p]
 * and eliminate the binding for [x] if [x] is not used.
 *)
| ATOM(x,a,p) -> optATOM env x (optAtom a) p
(*
 * If, after optimisation, we find that the atom [a] is a boolean constant
 * we eliminate the conditional, replacing it with [p] or [q] as appropriate.
 *)
| IF(a,p,q) ->
    (match optAtom a with
       BOOL true -> improved := true; optProc env p
     | BOOL false -> improved := true; optProc env q
     | a ->
         if mustFail p then
	   IF(a,optProc initEnv p,optProc env q)
	 else if mustFail q then
	   IF(a,optProc env p,optProc initEnv q)
	 else
	   finish (IF(a,optProc initEnv p,optProc initEnv q)) env)
(*
 * If, after optimising [p], we find that [!u] is false, then [x] is
 * not used in [p] and we can eliminate the channel [x].
 *)
| NEW(x,_,p) ->
    if Occur.oper x = 0 && Occur.arg x = 0 then optProc env p
    else NEW(x,REQUIRED,optProc env p)
(*
 * We note all the variables which are used in [inc], since we must ensure
 * that the bindings for those variables are not eliminated as dead code.
 *)
| STRUCT(inc) ->
   let inc = Types.mapBindings (fun (ty,a) -> (ty,optAtom a)) inc in
   finish (STRUCT inc) env
(*
 * The null process does not add anything to the normal form.
 *)
| SKIP -> finish SKIP env
| OUTPUT(x,y,_) ->
    (let x = optAtom x in let y = optAtom y in
     match findMatch x env.inp with
       Some(a,inp) -> optSend {inp=inp; out=env.out; rest=env.rest} a [y]
     | None -> finish SKIP {inp=env.inp; out=(x,y)::env.out; rest=env.rest})
(*
 * An input process cannot be optimised on its own, since we don't
 * know whether it has any potential communication partners (the channel
 * [ch] might be optimisable, however). The continuation [p] is optimised
 * in a context which records the fact that we (unfortunately) know
 * nothing about [arg].
 *)
| INPUT(x,y,_) ->
    (let x = optAtom x in let y = optAtom y in
     match findMatch x env.out with
       Some(a,out) -> optSend {inp=env.inp; out=out; rest=env.rest} y [a]
     | None -> finish SKIP {inp=(x,y)::env.inp; out=env.out; rest=env.rest})
| SEND(x,l) ->
    optSend env (optAtom x) (List.map optAtom l)

and optATOM env x a p =
  match a with
    PROJECT(_) ->
      (match (Occur.oper x + Occur.arg x + Occur.proj x) with
         0 -> optProc env p
       | 1 -> Hashtbl.add atomTable x a; optProc env p
       | _ -> ATOM(x,a,optProc env p))
  | _ ->
      Hashtbl.add atomTable x a; optProc env p

and optSend env a l = match a with
  DYNAMIC x -> tryInline env a x l
| STATIC x -> tryInline env a x l
| PROJECT(_) -> finish (SEND(a,l)) env
| COERCION(_) | ADDR(_) | CHAR(_) | BOOL(_) | INT(_) | CCONST(_) ->
    Error.bug "Optimise.optSend"

(*
 * If ch is known to be a process definition, then it must be inlinable
 * (since we only inlinable process definitions in the table).  We therefore
 * substitute the actual arguments l for the bound args and continue
 * optimising p.
 *
 * If ch is known to be a tuple, then that means we are outputting to a
 * process definition which has already been closure converted.  We therefore
 * convert the output to a static output, passing the closure explicitly.
 * We continue optimising the static output, just in case the static
 * process definition is inlinable.
 *)
and tryInline env a x l =
  match tupleInfo x with
    Some(ADDR f :: _) ->
      (match defInfo f with
         None -> finish (SEND(a,l)) env
       | Some(clone,args,p) -> inlineDef env clone args (a :: l) p)
  | Some(_) ->
      Error.bug "Optimise.tryInline"
  | None ->
      (match defInfo x with
         Some(clone,args,p) -> inlineDef env clone args l p
       | None -> finish (SEND(a,l)) env)

and inlineDef env clone args l p =
  let (args,p) = if clone then Inter.clone args p else (args,p) in
  improved := true;
  let addArg o a =
    match o with None -> None | Some x -> Hashtbl.add atomTable x a; Some(x,a)
  in let addArgAtom p o =
    match o with None -> p | Some(x,a) -> ATOM(x,a,p)
  in
    let l = List.map2 addArg args l in
    List.fold_left addArgAtom (optProc env p) l

and finish proc env = match env.rest with
  [] ->
    List.fold_left (fun p (x,y) -> PRL(OUTPUT(x,y,UNKNOWN),p))
    (List.fold_left (fun p (x,y) -> PRL(INPUT(x,y,UNKNOWN),p))
     proc env.inp)
    env.out
| p :: rest ->
    let p = optProc {inp=env.inp; out=env.out; rest=rest} p in
    (match proc with SKIP -> p | _ -> PRL(proc,p))

exception FoundStruct

let rec scanProc = function
  PRL(p,q) -> scanProc p; scanProc q
| VAL(l,p) -> List.iter (scanVal true) l; scanProc p
| CCODE(_,_,_,_,p) -> scanProc p
| CCALL(_,_,_,_,p) -> scanProc p
| ATOM(_,_,p) -> scanProc p
| NEW(_,_,p) -> scanProc p
| STRUCT(_) -> raise FoundStruct
| IF(_) | SKIP | OUTPUT(_) | INPUT(_) | SEND(_) -> ()

and scanVal extern = function
  TUPLE(x,l) -> Hashtbl.add tupleTable x l
| DEF(x,_,_,p) -> scanProc p
| STRING(x,s) -> Hashtbl.add atomTable x (STATIC (storeString extern x s))

let scanImport prog =
  List.iter scanConst prog.constants;
  try scanProc prog.proc with FoundStruct -> ()

let optProg imports prog =
  List.iter scanImport imports;
  List.iter (scanVal true) prog.constants;
  let p = optProc initEnv prog.proc in
  let prog =
    {includes = prog.includes; static = prog.static;
     constants = addStrings prog.constants; proc = p}
  in
    Hashtbl.clear atomTable; Hashtbl.clear tupleTable;
    Hashtbl.clear defTable; Hashtbl.clear stringTable;
    prog  

(*
 * [show n l defs] prints out the process definitions [defs] and adds
 * their code statistics to [l]. [n] is the of the optimiser pass
 * which produced [defs].
 *)

let show c n stats imports prog =
  if !showInter then
    (Format.print_string "########## "; Format.print_string n;
     Format.print_string " ##########"; Format.print_newline();
     Inter.print printOccur prog);
  if !lint then Lint.check c imports prog :: stats else stats

(*
 * This function repeatedly runs the optimiser until it fails to make
 * any further improvements (the optimiser sets the variable [improved]
 * to [true] whenever it makes an improvement). If required, it prints out
 * the optimised code after each pass and records code statistics. If
 * the fixpointOptimise flag is false, we stop after four passes.
 *)

let rec keepOptimising stats imports prog pass =
  if !optimise then
    begin
      let c = Char.chr (Char.code '0' + pass) in
      let name = "Optimisation " ^ string_of_int pass in
      improved := false; Stats.mark name;
      let prog = optProg imports prog in
      Occur.occur imports prog;
      let stats = show c name stats imports prog in
      if !improved & (!fixpointOptimise or pass < 3) then
	keepOptimising stats imports prog (pass+1)
      else
	(stats,prog)
    end
  else
    (stats,prog)

(*
 * The top-level optimisation function simply calls [keepOptimising]
 * and prints out the optimised code and code statistics (if required).
 *)

let optimise imports prog =
  if !showImports then
    (Format.print_string "########## Imports ##########";
     Format.print_newline(); List.iter (Inter.print (fun _ -> ())) imports);
  Occur.occur imports prog;
  let (stats,prog) =
    keepOptimising (show 'I' "Initial program" [] imports prog) imports prog 1
  in let (stats,prog) =
    Stats.mark "Calculating free variables";
    let prog = Fv.fv imports prog in
    (show 'C' "Closure conversion" stats imports prog,prog)
  in let (stats,prog) =
    Stats.mark "Channel status inference";
    let prog = Status.infer imports prog in
    (show 'S' "Channel status inference" stats imports prog,prog)
in
  if !showOptimisedInter & not(!showInter) then
    (Format.print_string "Intermediate code after all optimisations:\n";
     Inter.print printOccur prog);
  if !showCodeStats then Lint.format (List.rev stats);
  prog
