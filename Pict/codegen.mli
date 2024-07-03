(*
 * A compilation from Core Pict into C.
 *)

val profiling : bool ref
val trans : Inter.toplevel list -> Inter.toplevel -> Ccode.decl list
val constants : Inter.toplevel list -> Inter.toplevel -> Ccode.decl list
