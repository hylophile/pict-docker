(*
 * A "debugging wrapper" for optionally printing the arguments and
 * results to functions, maintaining proper nesting and indentation.
 *)

val wrap : 
     string             (* name of debugged function *)
  -> (unit -> 'a)       (* thunk containing underlying function *)
  -> (unit -> unit)     (* called before underlying fn to show args *)
  -> ('a -> unit)       (* used to show result returned by underlying fn *)
  -> 'a

(*
 * wrapCont is similar to wrap, except that it wraps a printing function
 * around a continuation-passing function definition.
 *)

val wrapCont : 
     string              (* name of debugged function *)
  -> (('a -> 'b) -> 'b)  (* closure containing underlying function *)
  -> (unit -> unit)      (* called before underlying fn to show args *)
  -> ('a -> unit)        (* used to show result before calling cont *)
  -> ('a -> 'b)          (* the continuation *)
  -> 'b
