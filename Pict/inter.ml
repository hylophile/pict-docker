(*
 * Intermediate code.
 *)

let exactInter =
  Flags.createFlag "exactInter"
  "*Show exact representation of intermediate code" false

type status =
  EMPTY
| REPLICATED
| ONEREADER
| ONEWRITER
| KNOWN
| UNKNOWN

let unionStatus a b =
  match (a,b) with
    (EMPTY,EMPTY) -> EMPTY
  | (REPLICATED,REPLICATED) -> REPLICATED
  | (ONEREADER,ONEREADER) -> ONEREADER
  | (ONEWRITER,ONEWRITER) -> ONEWRITER
  | (KNOWN,KNOWN) -> KNOWN
  | (_,_) -> UNKNOWN

let printStatus = function
  EMPTY -> Format.print_string "{-e-}"
| REPLICATED -> Format.print_string "{-*-}"
| ONEREADER -> Format.print_string "{-r-}"
| ONEWRITER -> Format.print_string "{-w-}"
| KNOWN -> Format.print_string "{-k-}"
| UNKNOWN -> ()

let interStatus a b =
  match (a,b) with
    (EMPTY,EMPTY) -> EMPTY
  | (REPLICATED,REPLICATED) -> REPLICATED
  | (ONEREADER,ONEREADER) -> ONEREADER
  | (ONEWRITER,ONEWRITER) -> ONEWRITER
  | (KNOWN,KNOWN) -> KNOWN
  | (UNKNOWN,s) -> s
  | (s,UNKNOWN) -> s
  | (KNOWN,s) -> s
  | (s,KNOWN) -> s
  | (_,_) -> Error.bugf (fun () ->
      Format.print_string "interStatus: inconsistent status values: ";
      printStatus a; Format.print_string " and "; printStatus b
    )

type inlinable = INLINABLE | NOTINLINABLE
type init = REQUIRED | NOTREQUIRED

type proc =
    PRL of proc * proc
  | VAL of value list * proc
  | CCODE of Var.var option * Ccode.info * string list * atom list * proc
  | CCALL of Var.var option * Ccode.info * string * atom list * proc
  | ATOM of Var.var * atom * proc
  | IF of atom * proc * proc
  | NEW of Var.var * init * proc
  | INPUT of atom * atom * status
  | OUTPUT of atom * atom * status
  | SEND of atom * atom list
  | STRUCT of atom Types.c
  | SKIP 

and atom =
    INT of int
  | CHAR of char
  | BOOL of bool
  | ADDR of Var.var
  | STATIC of Var.var
  | DYNAMIC of Var.var
  | PROJECT of atom * int
  | CCONST of Ccode.info * string
  | COERCION of Ccode.coercion * atom

and value =
    TUPLE of Var.var * atom list
  | DEF of Var.var * inlinable * Var.var option list * proc
  | STRING of Var.var * string

type toplevel =
  {includes : string list;
   static : string list;
   constants : value list;
   proc : proc}

let small = function
  INPUT(_) | OUTPUT(_) | SEND(_) | SKIP -> true
| PRL(_) | VAL(_) | CCODE(_) | CCALL(_) | ATOM(_)
| IF(_) | NEW(_) | STRUCT(_) -> false

let formatBinder = function
  None -> Format.print_string "_"
| Some x -> Var.format x

let printCi ci =
  match ci.Ccode.kind with
    Ccode.EXP ->
    (match (ci.Ccode.const,ci.Ccode.reader,ci.Ccode.writer) with
       ( true,false,false) -> Format.print_string "C "
     | (false,false,false) -> Format.print_string "P "
     | (false, true,false) -> Format.print_string "R "
     | (false, true, true) -> Format.print_string "W "
     | (    _,    _,    _) -> Error.bug "Inter.printCCode: Bad purity")
  | Ccode.STATEMENT -> Format.print_string "E "

let rec printProc bi = function
  PRL(p,q) ->
    if small p or !exactInter then
      (Format.print_string "run"; Format.print_break 1 2;
       printBoxProc bi p; Format.print_space(); Format.print_string "end";
       Format.print_space(); printBoxProc bi q)
    else
      (Format.print_string "run"; Format.print_break 1 2;
       printBoxProc bi q; Format.print_space(); Format.print_string "end";
       Format.print_space(); printBoxProc bi p)
| VAL(l,p) ->
    printVals bi l; Format.print_space(); printProc bi p
| CCODE(None,ci,sl,l,p) ->
    Format.open_hovbox 0; printCCode ci sl l; Format.close_box();
    Format.print_space(); printProc bi p
| CCODE(Some x,ci,sl,l,p) ->
    Format.open_hovbox 0; Format.print_string "val "; Var.format x; bi x;
    Format.print_string " ="; Format.print_space(); printCCode ci sl l;
    Format.close_box(); Format.print_space(); printProc bi p
| CCALL(None,ci,s,l,p) ->
    Format.open_hovbox 0; printCCall ci s l; Format.close_box();
    Format.print_space(); printProc bi p
| CCALL(Some x,ci,s,l,p) ->
    Format.open_hovbox 0; Format.print_string "val "; Var.format x; bi x;
    Format.print_string " ="; Format.print_space(); printCCall ci s l;
    Format.close_box(); Format.print_space(); printProc bi p
| ATOM(x,a,p) ->
    Format.open_hovbox 0; Format.print_string "val "; Var.format x; bi x;
    Format.print_string " ="; Format.print_space(); printAtom a;
    Format.close_box(); Format.print_space(); printProc bi p
| IF(a,p,q) ->
    Format.print_string "if";
      Format.print_break 1 2; printAtom a; Format.print_space();
    Format.print_string "then";
      Format.print_break 1 2; printBoxProc bi p; Format.print_space();
    Format.print_string "else";
      Format.print_break 1 2; printBoxProc bi q; Format.print_space();
    Format.print_string "end"
| NEW(x,REQUIRED,p) ->
    Format.print_string "new "; Var.format x; bi x;
    Format.print_space(); printProc bi p
| NEW(x,NOTREQUIRED,p) ->
    Format.print_string "NEW "; Var.format x; bi x;
    Format.print_space(); printProc bi p
| INPUT(x,y,s) ->
    printAtom x; Format.print_string "?"; printStatus s; printAtom y
| OUTPUT(x,y,s) ->
    printAtom x; Format.print_string "!"; printStatus s; printAtom y
| SEND(x,l) ->
    printAtom x; Format.print_string "!!";
    Misc.formatList "[" "]" " " printAtom l
| STRUCT(c) ->
    Format.open_hovbox 0;
      Format.print_string "(struct ";
      Format.print_break 1 1; Types.printC c; Format.close_box();
      Format.print_cut(); Format.print_string ")";
    Format.close_box()
| SKIP ->
    Format.print_string "skip"

and printBoxProc bi p =
  Format.open_hvbox 0; printProc bi p; Format.close_box()

and printAtom = function
  INT(i) -> Format.print_int i
| CHAR(c) ->
    Format.print_string "'"; Format.print_char c; Format.print_string "'"
| COERCION(co,a) ->
    Ccode.formatCoercion co; Format.print_char '('; printAtom a; Format.print_char ')'
| BOOL(b) -> Format.print_bool b
| ADDR(x) -> Format.print_char '&'; Var.format x
| STATIC(x) -> Format.print_char '%'; Var.format x
| DYNAMIC(x) -> Var.format x
| PROJECT(a,i) -> printAtom a; Format.print_char '.'; Format.print_int i
| CCONST(ci,s) -> printCCode ci [s] []

and printVals bi l =
  let rec pr needSep = function
    [] -> ()
  | h :: t ->
    if needSep then Format.print_cut();
    Format.open_hovbox 0; printVal bi h; Format.close_box();
    pr true t
in
  Format.open_hvbox 0; pr false l; Format.close_box()

and printVal bi = function
  TUPLE(x,l) ->
    Format.print_string "val "; Var.format x; bi x;
    Format.print_string " ="; Format.print_space();
    Misc.formatList "[" "]" " " printAtom l
| DEF(x,i,args,p) ->
    (match i with
       INLINABLE -> Format.print_string "inline "
     | NOTINLINABLE -> ());
    Format.print_string "def "; Var.format x; bi x;
    Misc.formatList " [" "]" " " formatBinder args;
    Format.print_string " >"; Format.print_break 1 2; printBoxProc bi p
| STRING(x,s) ->
    Format.print_string "val "; Var.format x; bi x;
    Format.print_string " ="; Format.print_space(); Format.print_string "\"";
    Format.print_string (String.escaped s); Format.print_string "\""

and printCCode ci sl l =
  let rec print sl al = match (sl,al) with
    ([s],[]) -> Format.print_string s
  | (s :: sl,a :: al) -> Format.print_string s; printAtom a; print sl al
  | _ -> Error.bug "printCCode: Wrong number of arguments in CCODE"
in
  Format.open_hbox();
  Format.print_string "(ccode ";
  Format.print_int ci.Ccode.alloc; Format.print_string " ";
  printCi ci; print sl l; Format.print_string ")";
  Format.close_box()

and printCCall ci s l =
  let rec print sep = function
    [] -> ()
  | a :: al -> if sep then Format.print_string " "; printAtom a; print true al
in
  Format.open_hbox();
  Format.print_string "(ccode ";
  Format.print_int ci.Ccode.alloc; Format.print_string " ";
  printCi ci; Format.print_string s; Format.print_string " ";
  print false l; Format.print_string ")";
  Format.close_box()

let printToplevel s = Format.print_string s; Format.print_newline()

let print bi prog =
  Format.print_string "{- Includes -}";
  Format.print_newline(); List.iter printToplevel prog.includes;
  Format.print_string "{- Static C Code -}";
  Format.print_newline(); List.iter printToplevel prog.static;
  Format.print_string "{- Constants -}";
  Format.print_newline(); printVals bi prog.constants; Format.print_newline();
  Format.print_string "{- Processes -}";
  Format.print_newline(); printBoxProc bi prog.proc; Format.print_newline()

let rec join c p = match p with
  STRUCT(_) -> c
| PRL(p,q) -> PRL(join c p,join c q)
| VAL(l,p) -> VAL(List.map (joinVal c) l,join c p)
| CCODE(x,ci,sl,l,p) -> CCODE(x,ci,sl,l,join c p)
| CCALL(x,ci,s,l,p) -> CCALL(x,ci,s,l,join c p)
| ATOM(x,a,p) -> ATOM(x,a,join c p)
| NEW(x,i,p) -> NEW(x,i,join c p)
| IF(_) | INPUT(_) | OUTPUT(_) | SEND(_) | SKIP -> p

and joinVal c v = match v with
  DEF(x,i,args,p) -> DEF(x,i,args,join c p)
| TUPLE(_) | STRING(_) -> v

let joinImports imports prog =
  let p =
    List.fold_right (fun prog q -> join q prog.proc) imports
    (join SKIP prog.proc)
  in
    {includes = prog.includes; static = prog.static;
     constants = prog.constants; proc = p}

let cloneTable = (Hashtbl.create 200 : (Var.var,Var.var) Hashtbl.t)
let clone x = let y = Var.fresh() in Hashtbl.add cloneTable x y; y
let cloneOpt = function None -> None | Some x -> Some(clone x)
let cloneVar x = try Hashtbl.find cloneTable x with Not_found -> x

let rec cloneProc = function
  PRL(p,q) -> PRL(cloneProc p,cloneProc q)
| VAL(l,p) -> List.iter scanVal l; VAL(List.map cloneVal l,cloneProc p)
| IF(a,p,q) -> IF(cloneAtom a,cloneProc p,cloneProc q)
| STRUCT(inc) -> STRUCT(Types.mapBindings (fun (ty,a) -> (ty,cloneAtom a)) inc)
| CCODE(o,ci,sl,l,p) ->
    let o = cloneOpt o in CCODE(o,ci,sl,List.map cloneAtom l,cloneProc p)
| CCALL(o,ci,s,l,p) ->
    let o = cloneOpt o in CCALL(o,ci,s,List.map cloneAtom l,cloneProc p)
| ATOM(x,a,p) -> let x = clone x in ATOM(x,cloneAtom a,cloneProc p)
| NEW(x,i,p) -> let x = clone x in NEW(x,i,cloneProc p)
| INPUT(x,y,s) -> INPUT(cloneAtom x,cloneAtom y,s)
| OUTPUT(x,y,s) -> OUTPUT(cloneAtom x,cloneAtom y,s)
| SEND(x,l) -> SEND(cloneAtom x,List.map cloneAtom l)
| SKIP -> SKIP

and scanVal = function
  DEF(x,_,_,_) -> clone x; ()
| TUPLE(x,_) -> clone x; ()
| STRING(x,_) -> clone x; ()

and cloneVal = function
  DEF(x,i,args,p) ->
    let args = List.map cloneOpt args in DEF(cloneVar x,i,args,cloneProc p)
| TUPLE(x,l) -> TUPLE(cloneVar x,List.map cloneAtom l)
| STRING(x,s) -> STRING(cloneVar x,s)

and cloneAtom a = match a with
  DYNAMIC(x) -> DYNAMIC(cloneVar x)
| PROJECT(a,i) -> PROJECT(cloneAtom a,i)
| COERCION(c,a) -> COERCION(c,cloneAtom a)
| INT(_) | CHAR(_) | BOOL(_) | ADDR(_) | STATIC(_) | CCONST(_) -> a

let clone args p =
  let args = List.map cloneOpt args in let p = cloneProc p in
  Hashtbl.clear cloneTable; (args,p)
