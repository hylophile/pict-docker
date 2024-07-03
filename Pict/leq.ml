(*
 * Types for PICT
 * ==============
 *)

open Types

let debug = Flags.createFlag "leq" "*Trace subtype checking" false

let witnesses = (Hashtbl.create 10 : (tag,t option) Hashtbl.t)

let isWitness tag =
  try Hashtbl.find witnesses tag; true with Not_found -> false

(*
 * Exception for internal use only, must be caught by all top-level functions.
 *)

exception Error of (unit -> unit)
let fail f = raise (Error f)

module Map = Types.Map

let expandTVar c s =
  let exp () = match s with
    TVAR(_,_,tag) ->
      (try
         (match Hashtbl.find witnesses tag with Some t -> t | None -> s)
       with Not_found ->
         (expose c s))
  | _ -> (expose c s)
in
  if !debug then
    Debug.wrap "expandTVar"
    exp
    (fun () -> printT s)
    (fun s -> printT s)
  else
    exp()

(*
 * Helper function which creates a fresh type variable W, and adds
 * assumptions W<B,X=W,Y=W, for a given constraint C and variables X and Y.
 *)

let makeEqual c ks ns tags kt nt tagt con =
  let tag = freshTag() in
  constr (constr (constr c tag kt con) tags ks (EQ(TVAR(ks,ns,tag))))
  tagt kt (EQ (TVAR(kt,nt,tag)))

(* 
 * Subtyping checker
 *)

let rec leq c s t = 
  try
    if !debug then
      Debug.wrap "leq"
      (fun () -> leqLight c s t)
      (fun () ->
         printT s; Format.print_space(); Format.print_string "<";
	 Format.print_space(); printT t)
      (fun () -> ())
    else
      leqLight c s t
  with
    Error(why1) -> fail(fun()->
      Format.print_string "since"; displayT s; Format.print_space();
      Format.print_string "is not a subtype of"; displayT t;
      Format.print_space(); why1())

and leqLight c s t = match (s,t) with
  (_, TOP) -> ()
| (BOT, _) -> ()
| (TVAR(_,_,tags), TVAR(_,_,tagt)) ->
    if tags = tagt then ()
    else leqExposed c (expandTVar c s) (expandTVar c t)
| (_,_) ->
    leqExposed c (expandTVar c s) (expandTVar c t)

and leqExposed c s t = match (s,t) with
(*
 * Any type is less than Top, or greater than Bot.
 *)
| (_, TOP) -> ()
| (BOT, _) -> ()
(*
 * Subtyping of primitive types. ^ is invariant, ? is covariant,
 * ! and !* are contravariant, ^ < !, ^ < ?, and Char < Int.
 *)
| (CHAN s, CHAN t) -> leqLight c s t; leqLight c t s
| (OCHAN s, OCHAN t) -> leqLight c t s
| (ICHAN s, ICHAN t) -> leqLight c s t
| (CHAN s, ICHAN t) -> leqLight c s t
| (CHAN s, OCHAN t) -> leqLight c t s
| (RCHAN s, RCHAN t) -> leqLight c t s
| (RCHAN s, OCHAN t) -> leqLight c t s
| (STRING, STRING) -> ()
| (CHAR, CHAR) -> ()
| (BOOL, BOOL) -> ()
| (INT, INT) -> ()
| (CHAR, INT) -> ()
(*
 * For recursive types, we do something slightly tricky.  First, we
 * try comparing the types for equivalence.  If this succeeds, we're
 * happy.  If not, then we try the Amber rule ("assuming the bound vars
 * are LEQ, show that the bodies are LEQ...").  We believe this strategy 
 * is complete, but it introduces a slight incompleteness in type
 * inference, since the first (unsuccessful) check may instantiate some
 * unification variables, and these will *not* be uninstantiated before
 * we try the Amber rule.  So if you want two REC types to stand in the
 * subtype relation because of the Amber rule, better make sure that there 
 * are no unification variables around!
 *)
| (REC(ks,ns,tags,s), REC(kt,nt,tagt,t)) -> 
  (try
     let c = makeEqual c ks ns tags kt nt tagt (IN Kind.PN)
     in leq c s t; leq c t s
   with Error(_) ->
     let c = constr (constr c tagt kt (IN Kind.PN)) tags ks (LT(TVAR(kt,nt,tagt)))
     in leq c s t)
(*
 * If s and t are identical type variables then we succeed. Otherwise,
 * we try and promote s so that we can continue.
 *)
| (TVAR(_,_,tags), TVAR(_,_,tagt)) ->
    if tags = tagt then () else
    (match (isWitness tags, isWitness tagt) with
       (false, false) ->
         (match promote c tags with
            Some s -> leq c s t
          | None -> fail (fun () ->
              Format.print_string "since"; displayT s; Format.print_space();
              Format.print_string "cannot be (promoted to) a subtype of";
              displayT t))
      | (true, false) -> Hashtbl.add witnesses tags (Some t)
      | (false, true) -> Hashtbl.add witnesses tagt (Some s)
      | (true, true) -> Error.bug "Match.matchExpanded")
| (TVAR(_,_,tags), t) ->
    if isWitness tags then
      Hashtbl.add witnesses tags (Some t)
    else
      (match promote c tags with
         Some s -> leq c s t
       | None -> fail (fun()->
           Format.print_string "since"; displayT s; Format.print_space();
           Format.print_string "cannot be (promoted to) a subtype of";
           displayT t)
      )
| (s, TVAR(_,_,tagt)) ->
    if isWitness tagt then Hashtbl.add witnesses tagt (Some s)
    else fail (fun () ->
      Format.print_string "since"; displayT s; Format.print_space();
      Format.print_string "is not a subtype of"; displayT t
    )
| (CON(ks,ls,s),CON(kt,lt,t)) ->
    let c =
      List.fold_left2
        (fun c (ns,tags,ps,ks) (nt,tagt,_,kt) ->
           makeEqual c ks ns tags kt nt tagt (IN ps)
        ) c ls lt
    in
      leq c s t
| (TAPP(_,s,ls), TAPP(_,t,lt)) -> 
    if Kind.leq (kindOf s) (kindOf t) then
      (leq c s t;
       let rec loop kl ls lt = match (kl,ls,lt) with
           ([], [], []) -> ()
         | ((Kind.PN,_)::kl, s::ls, t::lt) ->
             leq c s t; leq c t s; loop kl ls lt
         | ((Kind.POS,_)::kl, s::ls, t::lt) ->
             leq c s t; loop kl ls lt
         | ((Kind.NEG,_)::kl, s::ls, t::lt) ->
             leq c t s; loop kl ls lt
         | ((Kind.CON,_)::kl, s::ls, t::lt) ->
             loop kl ls lt
         | (_, _, _) -> Error.bug "Leq.leq.TAPP"
       in loop (Kind.domOf (kindOf t)) ls lt)
    else
      fail (fun () ->
        Format.print_string "since the kind of";
        displayT s; Format.print_space();
        Format.print_string "is not a subkind of the kind of";
        displayT t
      )
(*
 * Subtyping of records (new fields can only be added on the right)
 * (Note that types of the form (... with ...) are always converted
 * to record types by expose, so we do not need an explicit clause
 * for them.)
 *)
| (RECORD ls, RECORD lt) ->
  (let rec loop c i ls lt = match (ls,lt) with
     (_, []) -> ()
   | ([],_) ->
       fail (fun()->
         Format.print_string "since"; displayT t; Format.print_space();
         Format.print_string "has more fields than"; displayT s
       )
   | (ANON(ss) :: ls,ANON(tt) :: lt) ->
       (leq c ss tt; loop c (i+1) ls lt)
   | ((LABELED(ns,ss) :: ls), (LABELED(nt,tt) :: lt)) ->
        if ns <> nt then
          fail (fun()->
            Format.print_string "since the field "; 
            Format.print_string ns; Format.print_string " in";
            displayT s; Format.print_space();
            Format.print_string "is matched by ";
            Format.print_string nt; Format.print_string " in";
            displayT t)
        else
          (leq c ss tt; loop c (i+1) ls lt)
   | (TY(ns,tags,ks,cons) :: ls,TY(nt,tagt,kt,cont) :: lt) ->
       leqCon c ks cons kt cont;
       loop (makeEqual c ks ns tags kt nt tagt cons) (i+1) ls lt
   | (_,_) ->
       fail (fun()->
         Format.print_string "since the field at position ";
	 Format.print_int i; Format.print_string " in";
         displayT s; Format.print_string " does not match that in";
	 displayT t
       )
  in loop c 0 ls lt)
(*
 * If none of the above cases apply, then the types s and t
 * have different shapes and we fail.
 *)
| (s,t) -> fail (fun () ->
    Format.print_string "since"; displayT s; Format.print_space();
    Format.print_string "and"; displayT t; Format.print_space();
    Format.print_string "do not match"
  )

and leqCon c k1 c1 k2 c2 =
  if Kind.leq k1 k2 then
    match (c1,c2) with
      (_, IN _) -> ()
    | (EQ t1, EQ t2) -> leq c t1 t2; leq c t2 t1
    | (LT t1, LT t2) -> leq c t1 t2
    | (EQ t1, LT t2) -> leq c t1 t2
    | (_,_) ->
        fail (fun () ->
          Format.print_string "The constraint ";
          printCon k1 c1;
          Format.print_string " does not refine ";
          printCon k2 c2
        )
  else
    fail (fun () ->
      Format.print_string "The constraint ";
      printCon k1 c1; Format.print_string " has kind ";
      Kind.print k1; Format.print_space();
      Format.print_string "but the constraint ";
      printCon k2 c2; Format.print_string " has kind ";
      Kind.print k2
    )

(*
 * Top level leq functions, which catch the Error exception and
 * format a nice message.
 *)

let isLeq fi c s t f =
  try leq c s t with Error(why) ->
  Error.errfAt fi (fun () ->
    Format.open_hovbox 0; f(); Format.close_box();
    Format.print_space(); Format.open_hovbox 0; why(); Format.close_box()
  )

let leqConstr fi c k1 con1 k2 con2 f =
  try leqCon c k1 con1 k2 con2 with Error(why) ->
  Error.errfAt fi (fun () ->
    Format.open_hovbox 0; f(); Format.close_box();
    Format.print_space(); Format.open_hovbox 0; why(); Format.close_box()
  )
