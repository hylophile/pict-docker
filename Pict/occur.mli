(*
 * Occurrence counting and dead-code elimination.
 *)

val occur : Inter.toplevel list -> Inter.toplevel -> unit
val proj : Var.var -> int
val oper : Var.var -> int
val arg : Var.var -> int

(* The first number (proj) is (a conservative approximation of) the
number of times a value is the subject of a project operation (this
number should be zero for all values except tuple values).  The second
number (oper) records the number of times a value is used in operator
position (for a channel or process definition, this essentially
records the number of times we send on it).  The third (arg) records
the number of times the value appears in `argument position' (such as
inside a tuple value).  

This occurrence information is used by the optimizer to guide the
inlining process and the elimination of dead code.  If all three
occurrence counts for a value are zero, then we can throw it away
(assuming that it has no side-effects).  If a process definition is
used exactly once in operator position, with no other kinds of uses,
we always inline the process definition, regardless of its size
(without this information, inlining process definitions might cause
code explosion).  *)
