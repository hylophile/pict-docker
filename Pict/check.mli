(*
 * [check imports dec] works out the types of all the [imports]
 * and then typechecks [dec] in the resulting context.
 *)

val check : Inter.toplevel list -> Syntax.toplevel -> Simplify.toplevel
