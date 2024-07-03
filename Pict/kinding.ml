(*
 * Functions to check for well-kinded type expressions
 * (and to convert tyes into our internal format).
 *)

open Types

let checkDuplicates fi l1 l2 =
  let f seen = function
    LABELED(n,t) ->
      if Misc.StringSet.mem n seen then
	Error.errfAt fi (fun () ->
	  Format.print_string "Duplicate record label: ";
          Format.print_string n
        )
      else
	Misc.StringSet.add n seen
  | ANON(_) | TY(_) ->
      seen
  in let base =
    List.fold_left (fun s ft -> match ft with
        LABELED(n,_) -> Misc.StringSet.add n s
      | ANON(_) | TY(_) -> s
    ) Misc.StringSet.empty l1
  in
    List.fold_left f base l2

let rec wellKinded c p = function
  Syntax.TVAR(fi,n) ->
    (try
       let tag = Misc.StringMap.find n c.types in
       match Map.find tag c.constr with
         (k,IN pn) ->
           if Kind.subset p pn then TVAR(k,n,tag)
           else Error.errfAt fi (fun () ->
             Format.print_string "Type variable has wrong polarity for this context: ";
             Format.print_string n
           )
       | (k,_) -> TVAR(k,n,tag)
     with Not_found ->
       Error.errfAt fi (fun () ->
	 Format.print_string "Unbound type variable: "; Format.print_string n
       ))
| Syntax.CON(_,l,s) ->
    let (l,kl,c) =
      List.fold_left (fun (l,kl,c) (n,p,k) ->
        let tag = freshTag() in
	let c = abind (constr c tag k (IN p)) n tag in
	((n,tag,p,k) :: l, (p,k) :: kl, c)
      ) ([],[],c) l
    in let s = wellKinded c p s
    in CON(Kind.ARROW(List.rev kl,Types.kindOf s),List.rev l,s)
| Syntax.REC(fi,n,k,t) -> 
    let tag = freshTag() in
    let t = wellKinded (abind (constr c tag k (IN Kind.PN)) n tag) p t in
    if k <> (Types.kindOf t) then
      Error.errfAt fi (fun()->
	Format.print_string "Ill-kinded recursive type:";
	Format.print_space();
	Format.print_string "declared kind of bound variable "; 
	Format.print_string n; 
	Format.print_string " must be equal to kind of corresponding body")
    else
      REC(k,n,tag,t)
| Syntax.TAPP(fi,s,l) ->
    let s = wellKinded c p s in let ks = Types.kindOf s in
    begin
      match ks with
        Kind.ARROW(argl,k) ->
          TAPP(k,s,leqKindList fi c p l argl)
      | _ -> Error.errfAt fi (fun()->
          Format.print_string "Ill-kinded type application:";
          Format.print_space(); Format.print_string "type ";
          printT s; Format.print_string " has kind "; Kind.print ks;
          Format.print_string ","; Format.print_space();
          Format.print_string "not an arrow kind")
    end
| Syntax.TOP(_) -> TOP
| Syntax.BOT(_) -> BOT
| Syntax.RECORD(fi,[]) -> RECORD []
| Syntax.RECORD(fi,l) ->
    let (_,l) = List.fold_left (buildFieldType p) (c,[]) l in
    checkDuplicates fi [] l; RECORD(List.rev l)
| Syntax.WITH(fi,b,l) ->
    let b = wellKinded c p b in
    let (_,l) = List.fold_left (buildFieldType p) (c,[]) l in
    (match expose c b with
       RECORD(bl) -> checkDuplicates fi bl l; RECORD(bl @ List.rev l)
     | _ -> Error.errfAt fi (fun()->
         Format.print_string "Left-hand side of WITH expression"; 
         Format.print_space(); printT b; Format.print_space();
         Format.print_string "is not a record type"))
| Syntax.WHERE(fi,_,_) ->
    Error.bugAt fi "WHERE types not implemented"
| Syntax.RCHAN(fi,t) -> RCHAN(hasKindType c (Kind.negate p) t)
| Syntax.CHAN(fi,t) -> CHAN(hasKindType c Kind.PN t)
| Syntax.OCHAN(fi,t) -> OCHAN(hasKindType c (Kind.negate p) t)
| Syntax.ICHAN(fi,t) -> ICHAN(hasKindType c p t)
| Syntax.STRING(_) -> STRING
| Syntax.BOOL(_) -> BOOL
| Syntax.CHAR(_) -> CHAR
| Syntax.INT(_) -> INT

and buildFieldType p (c,l) = function
  Syntax.ANON(_,t) -> (c,ANON(hasKindType c p t) :: l)
| Syntax.LABELED(_,n,t) -> (c,LABELED(n,hasKindType c p t) :: l)
| Syntax.TY(_,n,con) ->
    let tag = freshTag() and (k,con) = wellKindedConstr c p con in
    (abind (constr c tag k con) n tag,TY(n,tag,k,con) :: l)

and wellKindedConstr c p = function
  Syntax.IN k -> (k,IN Kind.PN)
| Syntax.LT t -> let t = wellKinded c p t in (kindOf t,LT t)
| Syntax.EQ t -> let t = wellKinded c p t in (kindOf t,EQ t)

and leqKindList fi c p l kl = match (l,kl) with
  ([], []) -> []
| (t :: l, (pa,k) :: kl) ->
    let t = wellKinded c (Kind.modify p pa) t in
    if Kind.leq (Types.kindOf t) k then t :: leqKindList fi c p l kl
    else Error.errfAt fi (fun()->
      Format.print_string "Ill-kinded type argument.  The type";
      Format.print_space(); printT t; Format.print_space();
      Format.print_string "has kind "; Kind.print (Types.kindOf t);
      Format.print_string " but a typ[e of kind "; Kind.print k;
      Format.print_string " was expected"
    )
| (_, []) ->
    Error.errfAt fi (fun()->
      Format.print_string "Too many type arguments"
    )
| ([], _) ->
    Error.errfAt fi (fun()->
      Format.print_string "Too many few type arguments"
    )

and hasKindType c p tt =
  let t = wellKinded c p tt in
  let k = Types.kindOf t in
  if k = Kind.TYPE then t else
  Error.errfAt (Syntax.typeInfo tt) (fun () ->
    Format.print_string "Type "; printT t; Format.print_string " of kind ";
    Kind.print k; Format.print_space();
    Format.print_string "used in a context where only kind Type is allowed")

let wellKinded c t = wellKinded c Kind.POS t
let wellKindedConstr c t = wellKindedConstr c Kind.POS t
let hasKindType c t = hasKindType c Kind.POS t
let mkRecord fi l = checkDuplicates fi [] l; RECORD(l)
