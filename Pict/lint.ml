(*
 * These functions check the consistency of a program, and collect
 * various statistics about the code.  For each program, we check
 * the following conditions:
 *
 * NO UNBOUND VARIABLES:
 *   every variable mentioned in the program must be in scope
 *
 * ALL BOUND VARIABLES MUST BE UNIQUE:
 *   we maintain this invariant so that we can use a simplified
 *   implementation of substitution and inlining
 *
 * REFERENCES TO CONSTANTS ARE MARKED STATIC:
 *   Constant values are accessed in a slightly different way to
 *   values which are build dynamically.  We therefore check that
 *   all variable which refer to constant values are marked STATIC.
 *)

open Inter

type stats =
  {name : char; cond : int; tuple : int; ctuple : int;
   project : int; string : int; dynamic : int; ccode : int; new_init : int;
   new_noinit : int; def : int; cdef : int; input : int;
   inputk : int; output : int; outputk : int; send : int}

exception FoundStruct
exception LintError of string
let error s = raise (LintError s)

let cond = ref 0 and tuple = ref 0 and ctuple = ref 0
let project = ref 0 and string = ref 0 and dynamic = ref 0 and ccode = ref 0
let new_init = ref 0 and new_noinit = ref 0
let def = ref 0 and cdef = ref 0 and input = ref 0 and inputk = ref 0
let output = ref 0 and outputk = ref 0 and send = ref 0

let binders = Hashtbl.create 500
let inScope = Hashtbl.create 500

let push x =
  try
    Hashtbl.find binders x; error ("Duplicate binding for " ^ Var.name x)
  with Not_found ->
    Hashtbl.add inScope x ()

let pop x = Hashtbl.remove inScope x

let pushBinder = function None -> () | Some x -> push x
let popBinder = function None -> () | Some x -> pop x

let lintVar x =
  try Hashtbl.find inScope x with Not_found ->
  error ("Unbound variable " ^ Var.name x)

let rec lint imp = function
  PRL(p,q) -> lint imp p; lint imp q
| VAL(l,p) ->
    List.iter pushVal l; List.iter (lintVal imp false) l;
    lint imp p; List.iter popVal l    
| CCODE(None,ci,_,l,p) ->
    (match ci.Ccode.kind with Ccode.EXP -> error "EXP with no result"
     | _ -> incr ccode; List.iter lintAtom l; lint imp p)
| CCODE(Some x,ci,_,l,p) ->
    incr ccode; List.iter lintAtom l; push x; lint imp p; pop x
| CCALL(None,ci,_,l,p) ->
    (match ci.Ccode.kind with Ccode.EXP -> error "EXP with no result"
     | _ -> incr ccode; List.iter lintAtom l; lint imp p)
| CCALL(Some x,ci,_,l,p) ->
    incr ccode; List.iter lintAtom l; push x; lint imp p; pop x
| ATOM(x,a,p) -> lintAtom a; push x; lint imp p; pop x
| IF(a,p,q) -> incr cond; lintAtom a; lint imp p; lint imp q
| NEW(x,REQUIRED,p) -> incr new_init; push x; lint imp p; pop x
| NEW(x,NOTREQUIRED,p) -> incr new_noinit; push x; lint imp p; pop x
| INPUT(x,y,i) ->
    (match i with
       EMPTY | ONEWRITER -> incr inputk; lintAtom x; lintAtom y
     | ONEREADER | KNOWN | UNKNOWN -> incr input; lintAtom x; lintAtom y
     | REPLICATED -> error "bad status for INPUT")
| OUTPUT(x,y,i) ->
    (match i with
       EMPTY | ONEREADER | REPLICATED -> incr outputk; lintAtom x; lintAtom y
     | ONEWRITER | KNOWN | UNKNOWN -> incr output; lintAtom x; lintAtom y)
| SEND(x,l) -> incr send; lintAtom x; List.iter lintAtom l
| STRUCT(inc) ->
    Types.allBindings (fun (_,a) -> lintAtom a) inc;
    if imp then raise FoundStruct else ()
| SKIP -> ()

and pushVal = function
  TUPLE(x,_) -> push x
| DEF(x,_,_,_) -> push x
| STRING(x,_) -> push x

and popVal = function
  TUPLE(x,_) -> pop x
| DEF(x,_,_,_) -> pop x
| STRING(x,_) -> pop x

and lintVal imp const = function
  TUPLE(_,l) ->
    if const then incr ctuple else incr tuple; List.iter lintAtom l
| DEF(x,_,l,p) ->
    (if const then incr cdef else incr def;
     try
       List.iter pushBinder l; lint imp p; List.iter popBinder l
     with LintError(s) ->
       Error.bug ("lint: " ^ Var.name x ^ ": " ^ s))
| STRING(_,_) -> incr string

and lintAtom = function
  ADDR x -> lintVar x
| STATIC x -> lintVar x
| DYNAMIC x -> incr dynamic; lintVar x
| PROJECT(a,_) -> incr project; lintAtom a
| COERCION(_,a) -> incr project; lintAtom a
| INT(_) | CHAR(_) | BOOL(_) | CCONST(_) -> ()

and lintField (_,a) = lintAtom a

let lintImportVal prog = List.iter pushVal prog.constants
let lintImportProc prog = try lint true prog.proc with FoundStruct -> ()

let check name imports prog =
  try
    cond := 0; tuple := 0; ctuple := 0; project := 0; string := 0;
    dynamic := 0; ccode := 0; new_init := 0; new_noinit := 0; send := 0;
    def := 0; cdef := 0; input := 0; inputk := 0; output := 0; outputk := 0;
    List.iter lintImportVal imports;
    List.iter pushVal prog.constants;
    List.iter (lintVal false true) prog.constants;
    List.iter lintImportProc imports; lint false prog.proc;
    Hashtbl.clear binders; Hashtbl.clear inScope;
    {name = name; cond = !cond; tuple = !tuple;
     ctuple = !ctuple; project = !project; string = !string;
     dynamic = !dynamic; ccode = !ccode; new_init = !new_init;
     new_noinit = !new_noinit; def = !def;
     cdef = !cdef; input = !input; inputk = !inputk; output = !output;
     outputk = !outputk; send = !send}
  with LintError(s) ->
    Error.bug ("lint: " ^ s)

let formatEntry f old s =
  let n = f s
in
  if n <> old then
    (Format.print_string(Printf.sprintf " %5d" (n-old)); n)
  else
    (Format.print_string "     ."; n)

let format l =
  let format str f =
    Format.print_string str;
    List.fold_left (formatEntry f) 0 l;
    Format.print_cut()
in
  Format.open_vbox 0;
  Format.print_string "                 ";
  List.iter (fun s -> Format.print_string "     "; Format.print_char s.name) l;
  Format.print_cut();
  Format.print_string "-----------------";
  List.iter (fun _ -> Format.print_string "------") l;
  Format.print_cut();
  format "New              " (fun s -> s.new_init);
  format "New (no init)    " (fun s -> s.new_noinit);
  format "Def              " (fun s -> s.def);
  format "Def (static)     " (fun s -> s.cdef);
  format "If               " (fun s -> s.cond);
  format "Input            " (fun s -> s.input);
  format "Input (static)   " (fun s -> s.inputk);
  format "Output           " (fun s -> s.output);
  format "Output (static)  " (fun s -> s.outputk);
  format "Send             " (fun s -> s.send);
  format "Tuple            " (fun s -> s.tuple);
  format "Tuple (static)   " (fun s -> s.ctuple);
  format "Project          " (fun s -> s.project);
  format "String           " (fun s -> s.string);
  format "Dynamic          " (fun s -> s.dynamic);
  format "CCode            " (fun s -> s.ccode);
  Format.close_box()
