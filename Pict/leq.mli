(*
 * Subtyping.
 *)

open Types

exception Error of (unit -> unit)
val witnesses : (tag,t option) Hashtbl.t
val leq : 'a c -> t -> t -> unit

val isLeq :
  Error.info -> 'a c -> t -> t -> (unit -> unit) -> unit

val leqConstr :
  Error.info -> 'a c -> Kind.kind -> constr -> Kind.kind -> constr ->
  (unit -> unit) -> unit
