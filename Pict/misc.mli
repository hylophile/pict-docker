
(* Integer operations *)

val iterateInt: int -> int -> int -> (int -> 'a -> 'a) -> 'a -> 'a

(* List operations *)

val itFold: (int -> 'a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val formatList : string -> string -> string -> ('a -> unit) -> 'a list -> unit
val printList :
  string -> string -> string -> (out_channel -> 'a -> unit) ->
  out_channel -> 'a list -> unit

(* Ready-built modules *)

module StringSet : Set.S with type elt = string

(* Extension of standard Map module *)

module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val union : 'a t -> 'a t -> 'a t
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val format : string -> string -> string -> (key -> 'a -> unit) -> 'a t -> unit
  end

module Make (Ord:Map.OrderedType): (S with type key = Ord.t)

(*
 * Ready-build map implementations
 *)

module StringMap : S with type key = string
module IntMap : S with type key = int
