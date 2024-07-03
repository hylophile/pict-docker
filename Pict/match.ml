(*
 * Type matching.
 *)

open Types

let debug = Flags.createFlag "match" "*Trace matching" false

let witnessError () =
  Format.print_string "Witness type is not a subtype of the required bound"

let rec scan fi c tl rtl = match (tl,rtl) with
  ([],[]) -> ([],[])
| (ANON t :: tl, ANON rt :: rtl) ->
    Leq.leq c t rt; scan fi c tl rtl
| (LABELED(n,t) :: tl, LABELED(rn,rt) :: rtl) ->
    if n<>rn then Error.errfAt fi (fun () -> 
      Format.print_string "Expected label "; Format.print_string rn; 
      Format.print_string " but found "; Format.print_string n);
    Leq.leq c t rt; scan fi c tl rtl
| (TY(_,_,k,EQ t) :: tl, TY(_,rtag,rk,EQ rt) :: rtl) ->
    if Kind.leq k rk then
      (Leq.leq c t rt; scan fi (Types.constr c rtag rk (EQ t)) tl rtl)
    else
      Error.errAt fi "Kinds do not match on type fields"
| (TY(_,_,k,EQ t) :: tl, TY(_,rtag,rk,LT rt) :: rtl) ->
    if Kind.leq k rk then
      (Leq.leq c t rt; scan fi (Types.constr c rtag rk (EQ t)) tl rtl)
    else
      Error.errAt fi "Kinds do not match on type fields"
| (TY(_,_,k,EQ t) :: tl, TY(_,rtag,rk,IN _) :: rtl) ->
    if Kind.leq k rk then
      scan fi (Types.constr c rtag rk (EQ t)) tl rtl
    else
      Error.errAt fi "Kinds do not match on type fields"
| (tl, TY(_,rtag,rk,EQ rt) :: rtl) ->
    scan fi (Types.constr c rtag rk (EQ rt)) tl rtl
| (tl, TY(_,rtag,rk,LT rt) :: rtl) ->
    (Hashtbl.add Leq.witnesses rtag None; scan fi c tl rtl)
| (tl,rtl) -> (tl,rtl)

let rec finalise fi c sub acc l tl rtl cont = match (l,tl,rtl) with
  (Simplify.ANONv v :: l, ANON _ :: tl, ANON _ :: rtl) ->
    finalise fi c sub (Simplify.ANONv v :: acc) l tl rtl cont
| (Simplify.ANONv v :: l, LABELED _ :: tl, LABELED _ :: rtl) ->
    finalise fi c sub (Simplify.ANONv v :: acc) l tl rtl cont
| (Simplify.TYv t :: l, TY _ :: tl, TY _ :: rtl) ->
    finalise fi c sub (Simplify.TYv t :: acc) l tl rtl cont
| (l, tl, TY(_,rtag,_,EQ rt) :: rtl) ->
    let ct = Types.subst rt sub in
    finalise fi c (Map.add rtag ct sub) (Simplify.TYv Simplify.BOXED :: acc)
    l tl rtl cont
| (l, tl, TY(_,rtag,rk,LT rt) :: rtl) ->
    (match Hashtbl.find Leq.witnesses rtag with
       None -> Error.errAt fi "Can't determine witness type"
     | Some t ->
         Leq.isLeq fi c t (Types.subst rt sub) witnessError;
         finalise fi c (Map.add rtag t sub)
         (Simplify.TYv Simplify.BOXED :: acc) l tl rtl cont)
| (l, tl, rtl) -> cont sub (List.rev acc) l tl rtl

let noExtraFields sub acc l tl rtl =
  match (l, tl, rtl) with
    ([],[],[]) -> acc
  | (_,_,_) -> Error.bug "Match.noExtraFields"

let record fi c l tl rtl =
  try
    Hashtbl.clear Leq.witnesses;
    match scan fi c tl rtl with
      ([], []) -> finalise fi c Map.empty [] l tl rtl noExtraFields
    | (_, []) ->
        Error.warningAt fi "Record has extra fields";
        finalise fi c Map.empty [] l tl rtl noExtraFields
    | ([], _) -> Error.errAt fi "Record does not have enough fields"
    | (_, _) -> Error.errAt fi "Record field mismatch"
  with Leq.Error(why) ->
    Error.errfAt fi (fun () ->
      Format.open_hovbox 0;
        Format.print_string "The synthesized record type";
        displayT (RECORD tl); Format.print_space();
        Format.print_string "does not match the expected record type";
        displayT (RECORD rtl);
      Format.close_box(); Format.print_space();
      Format.open_hovbox 0; why(); Format.close_box()
    )

let inferResult sub acc l tl rtl =
  match (l, tl, rtl) with
    ([],[],[Types.ANON t]) -> (acc, Types.subst t sub)
  | (_,_,_) -> Error.bug "Match.inferResult"

let app fi c l tl rtl =
  try
    Hashtbl.clear Leq.witnesses;
    match scan fi c tl rtl with
      ([], [Types.ANON _]) -> finalise fi c Map.empty [] l tl rtl inferResult
    | ([],_ :: _ :: _) -> Error.errAt fi "Too few arguments"
    | ([],_ :: _) -> Error.errAt fi "Illegal function type"
    | ([],[]) -> Error.errAt fi "Too many arguments"
    | (_ :: _,[]) -> Error.errAt fi "Too many arguments"
    | (_ :: _,_ :: _) -> Error.errAt fi "Record field mismatch"
  with Leq.Error(why) ->
    Error.errfAt fi (fun () ->
      Format.open_hovbox 0;
        Format.print_string "The synthesized argument type";
        displayT (RECORD tl); Format.print_space();
        Format.print_string "does not match the expected record type";
        displayT (RECORD rtl);
      Format.close_box(); Format.print_space();
      Format.open_hovbox 0; why(); Format.close_box()
    )
