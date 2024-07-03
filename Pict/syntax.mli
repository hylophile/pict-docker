(*
 * Abstract syntax.
 *)

type info = Error.info

type proc =
  PRL of info * proc * proc
| IF of info * value * proc * proc
| INPUT of info * value * pat * proc
| OUTPUT of info * value * value
| LET of info * dec * proc
| SKIP of info

and pat =
  RECORDp of info * fieldPat list
| ASp of info * string * ty option * pat
| VARp of info * string * ty option
| WILDp of info * ty option
| CONp of info * ty option * pat
| RECp of info * ty option * pat

and fieldPat =
  ANONp of info * pat
| LABELEDp of info * string * pat
| TYp of info * string * constr

and dec =
  DEF of info * (info * string * pat * proc) list
| INLINE of info * string * pat * proc
| NEW of info * string * ty
| VAL of info * pat * value
| SEQ of info * dec * dec
| RUN of info * proc

and ty =
  TOP of info
| BOT of info
| TVAR of info * string
| CON of info * (string * Kind.polarity * Kind.kind) list * ty
| REC of info * string * Kind.kind * ty
| TAPP of info * ty * ty list
| RECORD of info * fieldTy list
| WITH of info * ty * fieldTy list
| WHERE of info * ty * fieldTy list
| CHAN of info * ty
| OCHAN of info * ty
| ICHAN of info * ty
| RCHAN of info * ty
| STRING of info
| BOOL of info
| CHAR of info
| INT of info

and constr =
  IN of Kind.kind
| LT of ty
| EQ of ty

and fieldTy =
  ANON of info * ty
| LABELED of info * string * ty
| TY of info * string * constr

and value =
  CCODEv of info * Ccode.info * string list * fieldVal list
| CCALLv of info * Ccode.info * string * fieldVal list
| APPv of info * ty option * value * fieldVal list
| RECORDv of info * fieldVal list
| WITHv of info * ty option * value * fieldVal list
| WHEREv of info * ty option * value * fieldVal list
| STRINGv of info * string
| SELECTv of info * value * string
| VARv of info * string
| BOOLv of info * bool
| CHARv of info * char
| INTv of info * int
| LETv of info * dec * value
| RECv of info * value * ty option
| CONv of info * ty option * value
| IFv of info * ty option * value * value * value
| ABSv of info * pat * proc

and fieldVal =
  ANONv of info * value
| LABELEDv of info * string * value
| TYv of info * ty

type toplevel =
  {imports : (Error.info * string) list;
   static : string list;
   includes : string list;
   dec : dec}

(*
 * Extract file position information.
 *)

val procInfo: proc -> info
val patInfo: pat -> info
val valInfo: value -> info
val fvInfo: fieldVal -> info
val decInfo: dec -> info
val typeInfo: ty -> info
