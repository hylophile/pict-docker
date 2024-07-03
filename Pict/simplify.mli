(*
 * Fully-typed high-level Pict syntax.
 *)

type status = RESPONSIVE | UNKNOWN

type proc =
  PRL of proc * proc
| IF of value * proc * proc
| INPUT of value * pat * proc
| OUTPUT of status * value * value
| LET of dec * proc
| SKIP

and pat =
  RECORDp of fieldPat list
| ASp of Var.var * ty * pat
| VARp of Var.var * ty
| WILDp

and fieldPat =
  ANONp of pat
| TYp of Var.var

and dec =
  DEF of (Var.var * pat * proc) list
| INLINE of Var.var * pat * proc
| NEW of Var.var * ty
| VAL of pat * value
| SEQ of dec * dec
| RUN of proc

and ty =
  BOXED
| UNBOXED
| VAR of Var.var

and value =
  CCODEv of Ccode.info * string list * fieldVal list
| CCALLv of Ccode.info * string * fieldVal list
| COERCIONv of Ccode.coercion * value
| APPv of status * value * fieldVal list
| RECORDv of fieldVal list
| STRINGv of string
| SELECTv of value * int
| VARv of Inter.atom
| BOOLv of bool
| CHARv of char
| INTv of int
| LETv of dec * value
| IFv of value * value * value
| ABSv of pat * proc

and fieldVal =
  ANONv of value
| TYv of ty

type toplevel =
  {includes : string list;
   static : string list;
   types : Inter.atom Types.c;
   dec : dec}

val simplify : Inter.toplevel list -> toplevel -> Inter.toplevel
