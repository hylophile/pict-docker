(*
 * Intermediate code.
 *
 * We maintain the invariant that each Var is bound at most once in the
 * whole program. This allows us to avoid variable capture problems during
 * code generation.
 *)

(*
 * A value of type Status gives information about the possible states a
 * channel may be in.  We only keep track of status which can usefully
 * be exploited by the code generator.
 *)

type status =
    EMPTY          (* Empty                       *)
  | REPLICATED     (* Replicated                  *)
  | ONEREADER      (* One reader                  *)
  | ONEWRITER      (* One writer                  *)
  | KNOWN          (* Channel identity is known   *)
  | UNKNOWN        (* Channel identity is unknown *)

(*
 * unionStatus and interStatus computes the union and
 * intersection of two pieces of status information.
 *)

val unionStatus : status -> status -> status
val interStatus : status -> status -> status

(*
 * Indicates whether it is always desirable to inline a particular
 * process definition.  This attribute is currently provided by the user,
 * but should eventually be determined automatically (in most cases).
 *)

type inlinable = INLINABLE | NOTINLINABLE

(*
 * Indicates whether we should initialise a new channel when we allocate it.
 * (It might not be necessary to initialise the channel if we can guarantee
 * that the channel will be used immediately.)
 *)

type init = REQUIRED | NOTREQUIRED

type proc =
    PRL of proc * proc
  | VAL of value list * proc
  | CCODE of Var.var option * Ccode.info * string list * atom list * proc
  | CCALL of Var.var option * Ccode.info * string * atom list * proc
  | ATOM of Var.var * atom * proc
  | IF of atom * proc * proc
  | NEW of Var.var * init * proc
  | INPUT of atom * atom * status
  | OUTPUT of atom * atom * status
  | SEND of atom * atom list
  | STRUCT of atom Types.c
  | SKIP 

and atom =
    INT of int
  | CHAR of char
  | BOOL of bool
  | ADDR of Var.var
  | STATIC of Var.var
  | DYNAMIC of Var.var
  | PROJECT of atom * int
  | CCONST of Ccode.info * string
  | COERCION of Ccode.coercion * atom

and value =
    TUPLE of Var.var * atom list
  | DEF of Var.var * inlinable * Var.var option list * proc
  | STRING of Var.var * string

type toplevel =
    {includes : string list;
     static : string list;
     constants : value list;
     proc : proc}

val print : (Var.var -> unit) -> toplevel -> unit

(*
 * Substitutes an atom for a variable in a process. This function also
 * has the effect of alpha-renaming the process (to maintain our invariant
 * that each variable is bound at most once in the whole program).
 *)

val joinImports : toplevel list -> toplevel -> toplevel

val clone : Var.var option list -> proc -> Var.var option list * proc

(* ---------------------------------------------------------------------- *)

(*
  Some notes on the types defined above...

SEND is just like OUTPUT, except that we know that the channel we are
sending on is guaranteed to be a process definition.  The reason that we
have a separate SEND construct is that we can send multiple values to
process definitions (hence SEND takes an atom list, rather than an atom).

STRUCT is part of the implementation of separate compilation.  It records
which bindings are exported from a module.

After closure conversion, we distinguish between STATIC and DYNAMIC
variables (before closure conversion, every variable is DYNAMIC).  A STATIC
variable is one whose value is know to be a compile-time constant (such
variables are represented as global variables in the generated C code).
STATIC variables are important because we do not need to store them in
closures.  A DYNAMIC variable is one whose value we do not know at compile
time.

After closure conversion, all process definitions are closed (they have no
free DYNAMIC variables).  Each process definition in this form is
represented in C as a function.  ADDR(x) is a construct which yields the
address of such a function.
*)
    
