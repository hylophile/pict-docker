(*
 * Flags and commands, accessible dynamically by name.
 *)

val createFlag : string -> string -> bool -> bool ref
val createFlagIfEnabled : string -> string -> bool -> bool -> bool ref
val setFlag : Error.info -> bool -> string -> unit

val createCmd : string -> string -> (Error.info -> string list -> unit) -> unit
val doCmd : Error.info -> string -> (string list) -> unit
