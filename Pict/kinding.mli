(*
 * Functions to check for well-kinded type expressions
 * (and to convert tyes into our internal format).
 *)

val wellKinded : 'a Types.c -> Syntax.ty -> Types.t
val wellKindedConstr : 'a Types.c -> Syntax.constr -> Kind.kind * Types.constr
val hasKindType : 'a Types.c -> Syntax.ty -> Types.t
val mkRecord : Error.info -> Types.field list -> Types.t
