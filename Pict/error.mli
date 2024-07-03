(*
 * Command line parsing and error handling.
 *)

exception Exit of int

type info
val unknown : info
val progName : info

(*
 * Create file position info: filename lineno column
 *)
val create : string -> int -> int -> info
val printInfo : info -> unit

(*
 * Print an error message and fail.  The printing function is called
 * in a context where the formatter is processing an hovbox.  Insert
 * calls to Format.print_space to print a space or, if necessary,
 * break the line at that point.
 *)
val errf : (unit->unit) -> 'a
val errfAt : info -> (unit->unit) -> 'a

val err : string -> 'a
val errAt : info -> string -> 'a

val bug : string -> 'a
val bugf : (unit->unit) -> 'a
val bugfAt : info -> (unit->unit) -> 'a
val bugAt : info -> string -> 'a

val warning : string -> unit
val warningAt : info -> string -> unit

type 'a withinfo = {i: info; v: 'a}

val printErrorsAsIfInFile: string -> unit
