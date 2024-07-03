(*
 * Abstract C expressions (the very small fragment of C to which we compile).
 *)

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

(*
 * Coercions to/from C representations.
 *)

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

val printDecls : out_channel -> decl list -> unit
val formatDecls : decl list -> unit
val formatCoercion : coercion -> unit

(*
 * Various C code fragments used in the code generator.
 *)

val complexInput : exp -> exp -> code
val complexOutput : exp -> exp -> code
val sizeOf : exp -> exp
val free : exp
val startq : exp
val endq : exp
