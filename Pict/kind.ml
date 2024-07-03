(*
 * Kinds
 *)

type polarity =
    PN
  | POS
  | NEG
  | CON

type kind =
  TYPE
| ARROW of (polarity * kind) list * kind

let leqPolarity p1 p2 = match (p1,p2) with
  (  _, PN) -> true
| (POS,POS) -> true
| (NEG,NEG) -> true
| (CON,  _) -> true
| (_  ,  _) -> false

let rec leq k1 k2 = match (k1,k2) with
  (TYPE, TYPE) -> true
| (ARROW(l1,k1), ARROW(l2,k2)) -> leq k1 k2 & leqArgs l2 l1
| (_,_) -> false

and leqArgs l1 l2 = match (l1,l2) with
  ([], []) -> true
| ((p1,k1)::l1, (p2,k2)::l2) -> leqPolarity p1 p2 & leq k1 k2 & leqArgs l1 l2
| (_,_) -> false

let domOf = function
  TYPE -> Error.bug "Syntax.domOf: Tried to find domain of kind Type"
| ARROW(kl,_) -> kl

let printPolarity = function
  PN -> ()
| POS -> Format.print_string "Pos"
| NEG -> Format.print_string "Neg"
| CON -> Format.print_string "Con"

let negate = function
  PN -> PN
| POS -> NEG
| NEG -> POS
| CON -> CON

let subset p1 p2 = match (p1,p2) with
  (_  , PN) -> true
| (POS,POS) -> true
| (NEG,NEG) -> true
| (CON,  _) -> true
| (_  ,  _) -> false

let modify p1 p2 = match (p1,p2) with
  (_  ,CON) -> CON
| (CON,  _) -> CON
| (_  , PN) -> PN
| (PN ,  _) -> PN
| (POS,POS) -> POS
| (POS,NEG) -> NEG
| (NEG,NEG) -> POS
| (NEG,POS) -> NEG

let rec print = function
  TYPE -> Format.print_string "Type"
| ARROW(l,k) ->
  Misc.formatList "(" " -> " " " printArg l; print k; Format.print_string ")"

and printArg (p,k) =
  printPolarity p; Format.print_string " "; print k
