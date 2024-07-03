(*
 * Syntax utilities.
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

let procInfo = function
  PRL(i,_,_) -> i | IF(i,_,_,_) -> i | INPUT(i,_,_,_) -> i
| OUTPUT(i,_,_) -> i | LET(i,_,_) -> i | SKIP(i) -> i

let patInfo = function
  RECORDp(i,_) -> i | ASp(i,_,_,_) -> i | VARp(i,_,_) -> i
| WILDp(i,_) -> i | RECp(i,_,_) -> i | CONp(i,_,_) -> i

let valInfo = function
  CCODEv(i,_,_,_) -> i | CCALLv(i,_,_,_) -> i | APPv(i,_,_,_) -> i
| RECORDv(i,_) -> i | STRINGv(i,_) -> i | SELECTv(i,_,_) -> i | VARv(i,_) -> i
| BOOLv(i,_) -> i | CHARv(i,_) -> i | INTv(i,_) -> i | LETv(i,_,_) -> i
| RECv(i,_,_) -> i | IFv(i,_,_,_,_) -> i | WITHv(i,_,_,_) -> i
| WHEREv(i,_,_,_) -> i | ABSv(i,_,_) -> i | CONv(i,_,_) -> i

let fvInfo = function
  ANONv(i,_) -> i | LABELEDv(i,_,_) -> i | TYv(i,_) -> i

let decInfo = function
  DEF(i,_) -> i | INLINE(i,_,_,_) -> i | NEW(i,_,_) -> i
| VAL(i,_,_) -> i | SEQ(i,_,_) -> i | RUN(i,_) -> i

let typeInfo = function
  TVAR(i,_) -> i | CON(i,_,_) -> i | TAPP(i,_,_) -> i | TOP(i) -> i
| BOT(i) -> i | REC(i,_,_,_) -> i | RECORD(i,_) -> i | WITH(i,_,_) -> i 
| RCHAN(i,_) -> i | OCHAN(i,_) -> i | STRING(i) -> i
| BOOL(i) -> i | CHAN(i,_) -> i | CHAR(i) -> i | INT(i) -> i
| ICHAN(i,_) -> i | WHERE(i,_,_) -> i
