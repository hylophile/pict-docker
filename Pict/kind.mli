(*
 * Kinds
 *)

type polarity =
    PN
  | POS
  | NEG
  | CON

val negate : polarity -> polarity
val subset : polarity -> polarity -> bool
val modify : polarity -> polarity -> polarity

type kind =
  TYPE
| ARROW of (polarity * kind) list * kind

val leq : kind -> kind -> bool
val domOf : kind -> (polarity * kind) list
val printPolarity : polarity -> unit
val print : kind -> unit
