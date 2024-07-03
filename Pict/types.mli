(*
 * Types and contexts for the pi-calculus.
 *)

type tag
val freshTag : unit -> tag
val printTag : tag -> unit
module Map : Misc.S with type key = tag

type t =
  TOP
| BOT
| TVAR of Kind.kind * string * tag
| REC of Kind.kind * string * tag * t
| CON of Kind.kind * (string * tag * Kind.polarity * Kind.kind) list * t
| TAPP of Kind.kind * t * t list
| RECORD of field list  
| RCHAN of t
| OCHAN of t
| ICHAN of t
| CHAN of t
| STRING
| CHAR
| BOOL
| INT

and field =
  ANON of t
| LABELED of string * t
| TY of string * tag * Kind.kind * constr

and constr =
  IN of Kind.polarity
| LT of t
| EQ of t

type 'a c =
  {types: tag Misc.StringMap.t;        (* Maps type variables to unique tags *)
   constr: (Kind.kind * constr) Map.t; (* Maps tags to constraints *)  
   vars: (t * 'a) Misc.StringMap.t} (* Maps identifiers to types, plus an 'a *)

val empty : 'a c
val abind : 'a c -> string -> tag -> 'a c
val constr : 'a c -> tag -> Kind.kind -> constr -> 'a c
val xbind : string -> t -> 'a -> 'a c -> 'a c
val union : 'a c -> 'a c -> 'a c

val mkUnit : t

val kindOf : t -> Kind.kind

val typeOfX : Error.info -> string -> 'a c -> t * 'a
val isBoundVar : 'a c -> string -> bool
val isBoundTyVar : 'a c -> string -> bool
val lookupTyVar : 'a c -> tag -> Kind.kind * constr

val printT : t -> unit
val printCon : Kind.kind -> constr -> unit
val displayT : t -> unit
val printC : 'a c -> unit

val mapBindings : (t * 'a -> t * 'b) -> 'a c -> 'b c
val allBindings : (t * 'a -> unit) -> 'a c -> unit

val renameBoundVars : t -> t
val subst : t -> t Map.t -> t
val expose : 'a c -> t -> t
val promote : 'a c -> tag -> t option

val getCon : Error.info -> 'a c -> t -> t
val getRec : Error.info -> 'a c -> t -> t
val getIChan : Error.info -> 'a c -> t -> t
val getOChan : Error.info -> 'a c -> t -> t
val getFields : Error.info -> 'a c -> t -> field list
val getFieldType : Error.info -> 'a c -> t -> string -> t * int

val exposeCon : Error.info -> 'a c -> t -> t
val exposeRec : Error.info -> 'a c -> t -> t
val exposeFields : Error.info -> 'a c -> t -> field list
val exposeORChan : Error.info -> 'a c -> t -> t

val isChan : 'a c -> t -> bool
val isInt : 'a c -> t -> bool
val isBool : 'a c -> t -> bool

val leqRChan : 'a c -> t -> bool
val leqInt : 'a c -> t -> bool
val leqBool : 'a c -> t -> bool
val leqString : 'a c -> t -> bool
