
type var

val setModuleName : string -> unit

val main : var
val fresh : unit -> var
val local : unit -> var
val indexed : int -> var

val name : var -> string
val print : out_channel -> var -> unit
val format : var -> unit

module Map : Misc.S with type key = var
