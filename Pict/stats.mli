(*
 * Simple interface for collecting compiler statistics.
 * Each successive stage exectutes [mark "stagename"] which records
 * the current time. The compiler calls the [summary] function, which
 * prints out all the timing information.  Progress information can be
 * printed using the [progress] function.
 *)

val mark : string -> unit
val progress : string -> unit
val summary : unit -> unit
