(*
 * Functions for collectings a printing code statistics.
 *)

type stats

(*
 * [check s imports prog] collects code statistics for the program [prog].
 * [s] indicates which optimiser pass produced [prog].
 *)
val check : char -> Inter.toplevel list -> Inter.toplevel -> stats

(*
 * [format l] formats a list of code statistics using the standard
 * formatting stream abstraction.
 *)
val format : stats list -> unit
