(*
 * Matching.
 *)

val record : Error.info -> 'a Types.c ->
  Simplify.fieldVal list -> Types.field list -> Types.field list ->
  Simplify.fieldVal list

val app : Error.info -> 'a Types.c ->
  Simplify.fieldVal list -> Types.field list -> Types.field list ->
  Simplify.fieldVal list * Types.t
