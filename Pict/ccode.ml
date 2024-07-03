(*
 * Intermediate code.
 *)

let exact =
  Flags.createFlag "exactCCode"
  "*Show exact representation of abstract C code" false

type var =
  Var.var

type kind = EXP | STATEMENT

type info = {
  alloc: int;        (* Number of words allocated by the C code *)
  const: bool;       (* True if the C code is a constant *)
  reader: bool;      (* True if the C code reads from updatable storage *)
  writer: bool;      (* True if the C code writes to updatable storage *)
  kind: kind         (* Tells us what kind of expression this is *)
}

type coercion = INT2C | C2INT | BOOL2C | C2BOOL | STRING2C

type ty =
  INTEGER
| POINTER

type decl =
  BLOCK of var * (var * ty) list * var * var * int * code
| CONST of var * exp list
| BYTES of var * string
| TOPLEVEL of string
| FORWARD of var
| EXTERN of var

and code =
  SEQ of code * code
| ASSIGN of exp * exp
| IF of exp * code * code
| CODE of info * string list * exp list
| CALL of info * string * exp list
| PROFILING of string
| NULL

and exp =
  INT of int
| VAR of var
| ADDR of var
| OFFSET of bool * exp * int
| DEREF of var * int
| TAG of var * int
| TAGS of var
| INDEX of var * exp
| CCODE of info * string list * exp list
| CCALL of info * string * exp list
| COERCION of coercion * exp

let formatCoercion = function
  INT2C -> Format.print_string "I2C"
| C2INT -> Format.print_string "C2I"
| C2BOOL -> Format.print_string "C2B"
| BOOL2C -> Format.print_string "B2C"
| STRING2C -> Format.print_string "S2C"

let printDecls os l =
  let printInt = function
      -2 -> output_string os "-2"
    | -1 -> output_string os "-1"
    |  0 -> output_char os '0'
    |  1 -> output_char os '1'
    |  2 -> output_char os '2'
    |  3 -> output_char os '3'
    |  4 -> output_char os '4'
    |  5 -> output_char os '5'
    |  6 -> output_char os '6'
    |  7 -> output_char os '7'
    |  8 -> output_char os '8'
    |  9 -> output_char os '9'
    |  i -> output_string os (string_of_int i)
  in

  let rec printString os s x =
    if x = String.length s then output_string os "\\000" else
    let c = String.get s x in
    if Char.code c < 32 or Char.code c > 126 then
      (output_char os '\\';
       output_byte os ((Char.code '0') + ((Char.code c) lsr 6));
       output_byte os ((Char.code '0') + (((Char.code c) lsr 3) land 7));
       output_byte os ((Char.code '0') + ((Char.code c) land 7));
       printString os s (x+1))
    else if c = '\\' then
      (output_string os "\\\\"; printString os s (x+1))
    else if c = '"' then
      (output_string os "\\\""; printString os s (x+1))
    else
      (output_char os c; printString os s (x+1))
  in

  let printLocals os = function
    (x,INTEGER) -> Var.print os x
  | (x,POINTER) -> output_string os "*"; Var.print os x
  in

  let rec printCode = function
    SEQ(c,d) ->
      printCode c; printCode d
  | ASSIGN(e,f) ->
      printExp os e; output_string os "=";
      printExp os f; output_string os ";\n"
  | IF(e,c,NULL) ->
      output_string os "if("; printExp os e; output_string os "){\n";
      printCode c; output_string os "}\n"
  | IF(e,c,d) ->
      output_string os "if("; printExp os e; output_string os "){\n";
      printCode c; output_string os "}else{\n";
      printCode d; output_string os "}\n"
  | CODE(ci,sl,l) ->
      printCCode sl l; output_char os '\n'
  | CALL(ci,code,l) ->
      output_string os code; Misc.printList "(" ");\n" "," printExp os l
  | PROFILING(s) ->
      output_string os s
  | NULL ->
      if !exact then output_string os "/**/;\n" else ()

  and printExp os = function
    INT(i) ->
      printInt i
  | VAR(var) ->
      Var.print os var
  | ADDR(v) ->
      output_string os "(Val)&"; Var.print os v
  | INDEX(v,INT i) ->
      if i > 0 then
	(Var.print os v; output_char os '+'; printInt i)
      else if i < 0 then
	(Var.print os v; output_char os '-'; printInt (-i))
      else
	Var.print os v
  | INDEX(v,e) ->
      Var.print os v; output_char os '+'; printExp os e
  | TAG(v,0) ->
      output_string os "TAG("; Var.print os v; output_char os ')'
  | TAG(v,i) ->
      output_string os "TAG("; Var.print os v; output_char os '+';
      printInt i; output_char os ')'
  | TAGS(v) ->
      output_string os "TAG(&"; Var.print os v; output_char os ')'
  | DEREF(v,i) ->
      Var.print os v; output_char os '['; printInt i; output_char os ']'
  | OFFSET(m,e,i) ->
      if !exact && m then output_string os "/*mutable*/";
      output_string os "OFFSET("; printExp os e; output_char os ',';
      printInt i; output_char os ')'
  | CCALL(ci,code,l) ->
      output_string os code; Misc.printList "(" ")" "," printExp os l
  | CCODE(ci,sl,l) ->
      printCCode sl l
  | COERCION(INT2C,e) -> output_string os "I("; printExp os e; output_string os ")"
  | COERCION(C2INT,e) -> output_string os "intInt("; printExp os e; output_string os ")"
  | COERCION(BOOL2C,e) -> printExp os e
  | COERCION(C2BOOL,e) -> printExp os e
  | COERCION(STRING2C,e) -> output_string os "S("; printExp os e; output_string os ")"

  and printCCode sl l =
    match sl with [] -> () | (s :: sl) ->
    (match l with
       [] -> output_string os s
     | (e :: el) -> output_string os s; printExp os e; printCCode sl el)

  and printDecl = function
    BLOCK(var,l,free,endq,i,c) ->
      output_string os "void "; Var.print os var;
      (match l with [] -> output_string os "(void){\n" | _ ->
       Misc.printList "(void){\nVal " ";\n" "," printLocals os l);
      if i > 0 then
        (Var.print os free; output_string os "=Free;";
         Var.print os endq; output_string os "=EndQ;\n";
	 output_string os "if("; Var.print os free;
	 output_string os "+"; printInt i;
	 output_string os ">"; Var.print os endq;
	 output_string os "){Gc("; printInt i;
	 output_string os ");";
	 Var.print os free; output_string os "=Free;";
	 Var.print os endq; output_string os "=EndQ;}\n");
      printCode c; output_string os "}\n"
  | TOPLEVEL(s) ->
      output_string os s; output_char os '\n'
  | CONST(x,l) ->
      output_string os "const Val "; Var.print os x;
      output_string os "["; printInt(List.length l);
      Misc.printList "]={" "};\n" "," printExp os l
  | BYTES(x,s) ->
      let sz = String.length s + 1 in
      output_string os "const struct{Val h;char c["; printInt sz;
      output_string os "];}"; Var.print os x;
      output_string os "={STRING("; printInt sz; output_string os "),\"";
      printString os s 0; output_string os "\"};\n"
  | FORWARD(x) ->
      output_string os "void "; Var.print os x; output_string os "(void);\n"
  | EXTERN(x) ->
      output_string os "extern const Val "; Var.print os x;
      output_string os "[];\n"
in
  List.iter printDecl l

let formatDecls l = Format.print_flush(); printDecls stdout l

let complexOutput ch v =
  CODE(
    {alloc=9; const=false; reader=false; writer=true; kind=STATEMENT},
    ["ComplexOutput("; ","; ");"], [ch; v]
  )

let complexInput ch v =
  CODE(
    {alloc=9; const=false; reader=false; writer=true; kind=STATEMENT},
    ["ComplexInput("; ","; ");"], [ch; v]
  )

let pureExp sl l =
  CCODE({alloc=0; const=false; reader=false; writer=false; kind=EXP},sl,l)

let sizeOf v = pureExp ["SIZE("; ")"] [v]

let register x = pureExp [x] []
let free = register "Free"
let startq = register "StartQ"
let endq = register "EndQ"
