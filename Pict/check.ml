(*
 * Typechecking for the pi-calculus.
 *)

open Simplify

let check = Flags.createFlag "check" "*Debug typechecker" false

let verbose = Flags.createFlag "CHECK" "*Debug typechecker verbosely" false

let assertions =
  Flags.createFlag "assertions"
  "*Check all assertions made in the library code" false

let checks =
  Flags.createFlag "checks"
  "*Do all runtime checks specified in the library code" true

let debug = Flags.createFlag "debug"
  "*Include debugging code in libraries" false

let ochErr () =
  Format.print_string "Expected an output-channel type"
let ichErr () =
  Format.print_string "Expected an input-channel type"

let patErr () =
  Format.print_string "pattern's type does not match type of channel"

let matchErr () =
  Format.print_string "Expected type does not match actual"

let asErr () =
  Format.print_string
  "Pattern type is not a subtype of the given explicit type"
let conErr () =
  Format.print_string
  "Constructor pattern type does not match explicitly given type"
let recpErr () =
  Format.print_string
  "Recursive pattern type does not match explicitly given type"
let recordpErr () =
  Format.print_string
  "Record pattern type does not match annotation"
let outputErr () =
  Format.print_string "value's type does not match type of channel"
let ifErr () =
  Format.print_string "argument to conditional is not a boolean"
let thenElseErr () =
  Format.print_string
  "minimal types of 'then' and 'else' branches must be the same"
let appErr () =
  Format.print_string "argument has incorrect type"
let recordErr () =
  Format.print_string "Expected a record type containing the field "
let valPatErr () =
  Format.print_string "value does not match type of pattern"
let patValErr () =
  Format.print_string "pattern does not match type of value"
let foldErr () =
  Format.print_string "ill-typed fold"
let newErr () =
  Format.print_string "declared type is not a channel type"
let abbrvErr () =
  Format.print_string "type abbreviation fields are not identical"
let boundErr () =
  Format.print_string "bounds do not match on type fields"

exception NeedTypeAnnotation of Error.info

let printReqd = function
  None -> ()
| Some(t,_) -> Format.print_string "required type: "; Types.printT t

let checkReqd fi c t = function
  None -> t
| Some(reqdt,err) -> Leq.isLeq fi c t reqdt err; reqdt

let getReqd fi = function
  None -> Error.errAt fi "Type annotation required"
| Some(t,_) -> t

let getProvidedType fi c topt reqd =
  match (topt,reqd) with
    (None,None) -> raise (NeedTypeAnnotation fi)
  | (Some t,None) -> Kinding.hasKindType c t
  | (None,Some(t,_)) -> t
  | (Some t,Some(treqd,err)) -> 
    let t = Kinding.hasKindType c t in Leq.isLeq fi c treqd t err; t

let rec checkPat' topLevel c reqd = function
  Syntax.RECORDp(fi,l) ->
    (match reqd with
      None ->
        let (l,tl,inc) = synthFieldPat topLevel c l in
        (RECORDp l, Kinding.mkRecord fi tl, inc)
    | Some(reqdt,err) ->
        let tl = Types.getFields fi c reqdt in
        let (l,inc) = checkFieldPat topLevel fi c l tl in
        (RECORDp l, Kinding.mkRecord fi tl, inc))
| Syntax.ASp(fi,n,topt,pat) ->
    let (pat,t,inc) =
      match (reqd,topt) with
        (None,None) -> checkPat topLevel c None pat
      | (_,_) ->
	  let t = getProvidedType fi c topt reqd in
	  checkPat topLevel c (Some(t,asErr)) pat
    in
      if topLevel && Types.isBoundVar c n then
	Error.errAt fi "Attempt to rebind top-level identifier" 
      else
	let x = Var.fresh() in
	(ASp(x,BOXED,pat),t,
	 Types.union (Types.xbind n t (Inter.DYNAMIC x) Types.empty) inc)
| Syntax.VARp(fi,n,topt) ->
    let t = getProvidedType fi c topt reqd in
    if topLevel && Types.isBoundVar c n then
      Error.errAt fi "Attempt to rebind top-level identifier" 
    else
      let x = Var.fresh() in
      (VARp(x,BOXED),t,Types.xbind n t (Inter.DYNAMIC x) Types.empty)
| Syntax.WILDp(fi,topt) ->
    let t = getProvidedType fi c topt reqd in
    (WILDp,t,Types.empty)
| Syntax.RECp(fi,topt,pat) ->
    let t = getProvidedType fi c topt reqd in
    let reqd = Some(Types.getRec fi c t,recpErr) in
    let (pat,_,inc) = checkPat topLevel c reqd pat in
    (pat,t,inc)
| Syntax.CONp(fi,topt,pat) ->
    Error.errAt fi "CON"

and synthFieldPat topLevel c = function
  [] ->
    ([],[],Types.empty)
| Syntax.ANONp(_,pat) :: l ->
    let (pat,t,inc) = checkPat topLevel c None pat in
    let (l,tl,incs) = synthFieldPat topLevel c l in
    (ANONp pat :: l, Types.ANON t :: tl,Types.union inc incs)
| Syntax.LABELEDp(_,n,pat) :: l ->
    let (pat,t,inc) = checkPat topLevel c None pat in
    let (l,tl,incs) = synthFieldPat topLevel c l in
    (ANONp pat :: l, Types.LABELED(n,t) :: tl,Types.union inc incs)
| Syntax.TYp(fi,n,constr) :: l ->
    let tag = Types.freshTag() in
    let (k,constr) = Kinding.wellKindedConstr c constr in
    let (l,tl,incs) =
      synthFieldPat topLevel (Types.abind (Types.constr c tag k constr) n tag) l
    in
      if topLevel && Types.isBoundTyVar c n then
	Error.errAt fi "Attempt to rebind top-level type identifier" 
      else
	(TYp(Var.fresh()) :: l, Types.TY(n,tag,k,constr) :: tl,
	 Types.abind (Types.constr incs tag k constr) n tag)

and checkFieldPat topLevel fi c l tl = match (l,tl) with
  ([],[]) -> ([],Types.empty)
| (Syntax.ANONp(_,pat)::l, Types.ANON(t)::tl) ->
    let (pat,_,inc) = checkPat topLevel c (Some(t,matchErr)) pat in
    let (l,incs) = checkFieldPat topLevel fi c l tl in
    (ANONp pat :: l, Types.union inc incs)
| (Syntax.LABELEDp(fi,n,pat)::l, Types.LABELED(reqdn,reqdt)::tl) ->
    if n<>reqdn then Error.errfAt fi (fun () -> 
      Format.print_string "Expected label "; Format.print_string reqdn; 
      Format.print_string " but found "; Format.print_string n);
    let (pat,_,inc) = checkPat topLevel c (Some(reqdt,matchErr)) pat in
    let (l,incs) = checkFieldPat topLevel fi c l tl in
    (ANONp pat :: l, Types.union inc incs)
| (Syntax.TYp(fi,n,constr)::l, Types.TY(_,tag,rk,reqdConstr)::tl) ->
    let (k,constr) = Kinding.wellKindedConstr c constr in
    Leq.leqConstr fi c rk reqdConstr k constr boundErr;
    let c = Types.abind (Types.constr c tag k constr) n tag in
    let (l,incs) = checkFieldPat topLevel fi c l tl in
    if topLevel && Types.isBoundTyVar c n then
      Error.errAt fi "Attempt to rebind top-level type identifier" 
    else
      (TYp(Var.fresh()) :: l, Types.abind (Types.constr incs tag k constr) n tag)
| (_,_) ->
    Error.errAt fi "Field mismatch in record pattern"

and checkPat topLevel c reqd pat =
  if !check then
    Debug.wrap "checkPat"
    (fun () -> checkPat' topLevel c reqd pat)
    (fun () -> Error.printInfo (Syntax.patInfo pat); printReqd reqd)
    (fun (p,t,inc) -> Types.printT t; if !verbose then Types.printC inc)
  else
    checkPat' topLevel c reqd pat

let safeCheckPat topLevel c reqd pat =
  try checkPat topLevel c reqd pat with NeedTypeAnnotation(fi) ->
  Error.errAt fi "Type annotation required"

let leqReqd fi c topt reqd =
  match (topt,reqd) with
    (None,None) -> None
  | (Some t,None) -> Some(Kinding.hasKindType c t,matchErr)
  | (None,Some(_,_)) -> reqd
  | (Some t,Some(treqd,err)) -> 
    let t = Kinding.hasKindType c t in
    Leq.isLeq fi c t treqd err; Some(t,err)

let rec checkProc' c = function
  Syntax.PRL(_,p,q) ->
    PRL(checkProc c p,checkProc c q)
| Syntax.INPUT(fi,v,pat,p) ->
    let (v,t) = checkVal c None v in
    let argTy = Types.getIChan fi c t in
    let (pat,_,inc) = safeCheckPat false c (Some(argTy,matchErr)) pat in
    INPUT(v,pat,checkProc (Types.union c inc) p)
| Syntax.OUTPUT(fi,v,av) ->
    let (v,t) = checkVal c None v in
    let argTy = Types.getOChan fi c t in
    if Types.leqRChan c t then
      OUTPUT(RESPONSIVE,v,fst(checkVal c (Some(argTy,matchErr)) av))
    else
      OUTPUT(UNKNOWN,v,fst(checkVal c (Some(argTy,matchErr)) av))
| Syntax.SKIP(_) ->
    SKIP
| Syntax.LET(_,d,p) ->
    let (d,inc) = checkDec false c d in
    LET(d,checkProc (Types.union c inc) p)
| Syntax.IF(_,v,p,q) ->
    let (v,_) = checkVal c (Some(Types.BOOL,ifErr)) v in
    IF(v,checkProc c p,checkProc c q)

and checkVal' c reqd = function
  Syntax.CCODEv(fi,ci,sl,l) ->
    let (l,tl) = synthFieldVal c l in
    (match ci.Ccode.kind with
       Ccode.EXP -> (CCODEv(ci,sl,l), getReqd fi reqd)
     | Ccode.STATEMENT -> (CCODEv(ci,sl,l), checkReqd fi c Types.mkUnit reqd))
| Syntax.CCALLv(fi,ci,s,l) ->
    let convertResult t a =
      if Types.isInt c t then (COERCIONv(Ccode.C2INT,a),t) else
      if Types.isBool c t then (COERCIONv(Ccode.C2BOOL,a),t) else
      Error.errfAt fi (fun () ->
         Format.print_string "Expected an Int or Bool type but found ";
         Types.displayT t
      )
    in let rec convertArgs al tl = match (al,tl) with
        ([], []) -> []
      | (ANONv a :: al, Types.ANON t :: tl) ->
          if Types.leqInt c t then
	    ANONv(COERCIONv(Ccode.INT2C,a)) :: convertArgs al tl else
          if Types.leqBool c t then
	    ANONv(COERCIONv(Ccode.BOOL2C,a)) :: convertArgs al tl else
          if Types.leqString c t then
	    ANONv(COERCIONv(Ccode.STRING2C,a)) :: convertArgs al tl else
          Error.errfAt fi (fun () ->
             Format.print_string "Expected an Int,Bool or String type but found ";
             Types.displayT t
          )
      | (_,_) -> Error.bug "Check.covertArgs"
    in let (al,tl) = synthFieldVal c l in
    (match ci.Ccode.kind with
       Ccode.EXP ->
	 (match s with
	   "%async" ->
	      let async =
		try Sys.getenv "PICTASYNC" = "yes" with Not_found -> false
	      in
		(BOOLv async, checkReqd fi c Types.BOOL reqd)
	 | "%assertions" ->
	     (BOOLv (!assertions), checkReqd fi c Types.BOOL reqd)
	 | "%checks" ->
	     (BOOLv (!checks), checkReqd fi c Types.BOOL reqd)
	 | "%debug" ->
	     (BOOLv (!debug), checkReqd fi c Types.BOOL reqd)
         | _ ->
	     convertResult (getReqd fi reqd) (CCALLv(ci,s,convertArgs al tl)))
     | Ccode.STATEMENT ->
         (CCALLv(ci,s,convertArgs al tl), checkReqd fi c Types.mkUnit reqd))
| Syntax.RECORDv(fi,l) ->
    (match reqd with
       Some(t,_) ->
         let fields = Types.exposeFields fi c (Types.renameBoundVars t) in
         if List.length fields <> List.length l then
           let (l,tl) = synthFieldVal c l in
           (RECORDv(Match.record fi c l tl fields),t)
         else
           let (_,l) = checkFieldVal fi c l fields (noExtraFields fi) in
  	   (RECORDv l,t)
     | None ->
	 let (l,tl) = synthFieldVal c l in
	 (RECORDv l, Kinding.mkRecord fi tl))
| Syntax.WITHv(fi,topt,v,l) ->
    let (v,t) =
      match topt with
        None -> checkVal c None v
      | Some t -> checkVal c (Some(Kinding.hasKindType c t,matchErr)) v
    in let fields = Types.getFields fi c t in
    let (l,tl) = synthFieldVal c l in
    let var = Var.fresh() in
    let rec loop i accum =
      if i < 0 then
	LETv(VAL(VARp(var,BOXED),v),RECORDv accum)
      else
	loop (i-1) (ANONv(SELECTv(VARv(Inter.DYNAMIC var),i)) :: accum)
    in
      (loop (List.length fields - 1) l, Kinding.mkRecord fi (fields @ tl))
| Syntax.WHEREv(fi,topt,v,l) ->
    let (v,t) =
      match topt with
        None -> checkVal c None v
      | Some t -> checkVal c (Some(Kinding.hasKindType c t,matchErr)) v
    in let fields = Types.getFields fi c t in
    let (l,tl) = synthFieldVal c l in
    let rcd = Var.fresh() in
    let (d,map) =
      List.fold_left2 (fun (d,map) fv ft ->
	(match ft with
	  Types.LABELED(n,ty) ->
	    let var = Var.fresh() in
	    let (_,i) = Types.getFieldType fi c t n in
	    (match fv with
	       ANONv(v) ->
	         (SEQ(d,VAL(VARp(var,BOXED),v)),
		  Misc.IntMap.add i (ANONv(VARv(Inter.DYNAMIC var)),ft) map)
	     | TYv(_) ->
	         Error.bugAt fi "Check.checkVal: WHERE")
	| Types.ANON(_) -> Error.errAt fi "Expected a labeled value field"
	| Types.TY(_) -> Error.errAt fi "Expected a labeled value field")
      ) (VAL(VARp(rcd,BOXED),v),Misc.IntMap.empty) l tl
    in let (_,l,tl) =
      List.fold_right (fun ft (i,l,tl) ->
        let (fv,ft) =
	  try Misc.IntMap.find i map with Not_found ->
	  (ANONv(SELECTv(VARv(Inter.DYNAMIC rcd),i)), ft)
	in
	  (i-1, fv :: l, ft :: tl)
      ) fields (List.length fields-1,[],[])
    in
      (LETv(d,RECORDv l), checkReqd fi c (Types.RECORD tl) reqd)
| Syntax.APPv(fi,topt,v,al) ->
    let (v,t) = checkVal c None v in
    let r = if Types.leqRChan c t then RESPONSIVE else UNKNOWN in
    let operator = Types.renameBoundVars (Types.getOChan fi c t) in
    let fields = Types.exposeFields fi c operator in
    if List.length fields <> List.length al + 1 then
      let (l,tl) = synthFieldVal c al in
      let (l,last) = Match.app fi c l tl fields in
      let res =
        match leqReqd fi c topt reqd with
	  None -> Types.exposeORChan fi c last
        | Some(reqdt,err) ->
            Leq.isLeq fi c (Types.exposeORChan fi c last) reqdt err; reqdt
      in (APPv(r,v,l), res)
    else
      let (res,l) = checkFieldVal fi c al fields (checkResultType fi topt reqd)
      in (APPv(r,v,l), res)
| Syntax.SELECTv(fi,v,n) ->
    let (v,t) = checkVal c None v in
    let (ft,i) = Types.getFieldType fi c t n in
    (SELECTv(v,i), checkReqd fi c ft reqd)
| Syntax.STRINGv(fi,s) ->
    (STRINGv(s), checkReqd fi c Types.STRING reqd)
| Syntax.CHARv(fi,x) ->
    (CHARv x, checkReqd fi c Types.CHAR reqd)
| Syntax.VARv(fi,x) ->
    let (xt,a) = Types.typeOfX fi x c in
    (VARv a, checkReqd fi c xt reqd)
| Syntax.BOOLv(fi,b) ->
    (BOOLv b, checkReqd fi c Types.BOOL reqd)
| Syntax.INTv(fi,i) ->
    (INTv i, checkReqd fi c Types.INT reqd)
| Syntax.LETv(fi,d,v) ->
    let (d,inc) = checkDec false c d in
    let (v,t) = checkVal (Types.union c inc) reqd v in
    (match reqd with
       None ->
         Error.warningAt fi "Result type for LETv might be wrong";
	 (LETv(d,v),t)
    | Some(reqdt,_) ->
	 (LETv(d,v),reqdt))
| Syntax.RECv(fi,v,topt) ->
    (match leqReqd fi c topt reqd with
       None -> Error.errAt fi "Type annotation required"
     | Some(t,_) ->
         let nreqd = Some(Types.exposeRec fi c t,foldErr) in
         (fst(checkVal c nreqd v),t))
| Syntax.CONv(fi,topt,v) ->
    Error.errAt fi "CON"
| Syntax.IFv(fi,topt,b,v1,v2) ->
    let (b,_) = checkVal c (Some(Types.BOOL,ifErr)) b in
    let (v1,t) = checkVal c (leqReqd fi c topt reqd) v1 in
    let (v2,_) = checkVal c (Some(t,thenElseErr)) v2 in
    (IFv(b,v1,v2),t)
| Syntax.ABSv(fi,pat,p) ->
    (match reqd with
      None ->
        let (pat,t,inc) = safeCheckPat false c None pat in
        (ABSv(pat,checkProc (Types.union c inc) p),Types.RCHAN t)
    | Some(reqdt,_) ->
        let t = Types.exposeORChan fi c reqdt in
        let (pat,_,inc) = safeCheckPat false c (Some(t,matchErr)) pat in
        (ABSv(pat,checkProc (Types.union c inc) p),reqdt))

and noExtraFields fi c l tl =
  match (l,tl) with
    ([],[]) -> Types.mkUnit
  | (l, []) ->
      Error.warningAt fi "Record has extra fields";
      synthFieldVal c l; Types.mkUnit
  | ([], _) -> Error.errAt fi "Record does not have enough fields"
  | (fv :: _,_) -> Error.errAt (Syntax.fvInfo fv) "Record field mismatch"

and checkResultType fi topt reqd c l tl =
  match (l,tl) with
    ([],[Types.ANON t]) ->
      (match leqReqd fi c topt reqd with
	None -> Types.exposeORChan fi c t
      | Some(reqdt,err) ->
          Leq.isLeq fi c (Types.exposeORChan fi c t) reqdt err; reqdt)
  | ([],_ :: _ :: _) -> Error.errAt fi "Too few arguments"
  | ([],_ :: _) -> Error.errAt fi "Illegal function type"
  | ([],[]) -> Error.errAt fi "Too many arguments"
  | (fv :: _,[]) -> Error.errAt (Syntax.fvInfo fv) "Too many arguments"
  | (fv :: _,_ :: _) -> Error.errAt (Syntax.fvInfo fv) "Record field mismatch"

and synthFieldVal c = function
  [] -> ([],[])
| Syntax.ANONv(_,v) :: l ->
    let (v,t) = checkVal c None v in
    let (l,tl) = synthFieldVal c l in
    (ANONv v :: l, Types.ANON t :: tl)
| Syntax.LABELEDv(_,n,v) :: l ->
    let (v,t) = checkVal c None v in
    let (l,tl) = synthFieldVal c l in
    (ANONv v :: l, Types.LABELED(n,t) :: tl)
| Syntax.TYv(fi,t) :: l ->
    let t = Kinding.wellKinded c t in
    let (l,tl) = synthFieldVal c l in
    (TYv BOXED :: l, Types.TY(":x",Types.freshTag(),Types.kindOf t,Types.EQ t) :: tl)

and checkFieldVal fi c l tl finalise = match (l,tl) with
  (Syntax.ANONv(_,v) :: l, Types.ANON t :: tl) ->
    let (v,_) = checkVal c (Some(t,matchErr)) v in
    let (res,l) = checkFieldVal fi c l tl finalise in
    (res, ANONv v :: l)
| (Syntax.LABELEDv(fi,n,v) :: l, Types.LABELED(reqdn,t) :: tl) ->
    if n<>reqdn then Error.errfAt fi (fun () -> 
      Format.print_string "Expected label "; Format.print_string reqdn; 
      Format.print_string " but found "; Format.print_string n);
    let (v,_) = checkVal c (Some(t,matchErr)) v in
    let (res,l) = checkFieldVal fi c l tl finalise in
    (res, ANONv v :: l)
| (Syntax.TYv(fi,t) :: l, Types.TY(_,tag,k,constr) :: tl) ->
    let t = Kinding.wellKinded c t in
    Leq.leqConstr fi c (Types.kindOf t) (Types.EQ t) k constr boundErr;
    let (res,l) =
      checkFieldVal fi (Types.constr c tag (Types.kindOf t) (Types.EQ t))
      l tl finalise
    in
      (Types.subst res (Types.Map.add tag t Types.Map.empty), TYv BOXED :: l)
| (l, tl) ->
    (finalise c l tl, [])

and checkDec' topLevel c = function
  Syntax.SEQ(_,d1,d2) ->
    let (d1,inc1) = checkDec topLevel c d1 in
    let (d2,inc2) = checkDec topLevel (Types.union c inc1) d2 in
    (SEQ(d1,d2), Types.union inc1 inc2)
| Syntax.DEF(_,l) ->
    let (inc,l) = List.fold_left (scanDef topLevel c) (Types.empty,[]) l in
    (DEF(List.map (checkDef (Types.union c inc)) l), inc)
| Syntax.INLINE(fi,n,pat,p) ->
    if topLevel && Types.isBoundVar c n then
      Error.errAt fi "Attempt to rebind top-level identifier" 
    else
      let x = Var.fresh() in
      let (pat,t,inc) = safeCheckPat false c None pat in
      (INLINE(x,pat,checkProc (Types.union c inc) p),
       Types.xbind n (Types.RCHAN t) (Inter.DYNAMIC x) Types.empty)
| Syntax.VAL(_,pat,v) ->
    (try
       let (pat,t,inc) = checkPat topLevel c None pat in
       let (v,_) = checkVal c (Some(t,valPatErr)) v in
       (VAL(pat,v), inc)
     with NeedTypeAnnotation(_) ->
       let (v,t) = checkVal c None v in
       let (pat,_,inc) = safeCheckPat topLevel c (Some(t,patValErr)) pat in
       (VAL(pat,v), inc))
| Syntax.NEW(fi,n,t) ->
    let x = Var.fresh() in
    let t = Kinding.hasKindType c t in
    if not(Types.isChan c t) then
      Error.errfAt fi (fun () ->
       Format.print_string "Expected an channel type but found ";
       Types.displayT t);
    (NEW(x,BOXED), Types.xbind n t (Inter.DYNAMIC x) Types.empty)
| Syntax.RUN(_,p) ->
    (RUN(checkProc c p), Types.empty)

and checkDef c (x,pat,inc,p) =
  (x,pat,checkProc (Types.union c inc) p)

and scanDef topLevel c (inc,l) (fi,n,pat,p) =
  if topLevel && Types.isBoundVar c n then
    Error.errAt (Syntax.patInfo pat) "Attempt to rebind top-level identifier" 
  else
    let x = Var.fresh() in
    let (pat,t,patInc) = safeCheckPat false c None pat in
    (Types.xbind n (Types.RCHAN t) (Inter.DYNAMIC x) inc,
     (x,pat,patInc,p) :: l)

(*
 * Debugging versions of some of the above
 *)  

and checkProc c p =
  if !check then
    Debug.wrap "checkProc"
    (fun () -> checkProc' c p)
    (fun () -> Error.printInfo (Syntax.procInfo p))
    (fun _ -> ())
  else
    checkProc' c p

and checkVal c reqd v = 
  if !check then
    Debug.wrap "checkVal"
    (fun () -> checkVal' c reqd v)
    (fun () -> Error.printInfo (Syntax.valInfo v); printReqd reqd)
    (fun (_,t) -> Types.printT t)
  else
    checkVal' c reqd v

and checkDec topLevel c d =
  if !check then
    Debug.wrap "checkDec"
    (fun cont -> checkDec' topLevel c d)
    (fun () -> Error.printInfo (Syntax.decInfo d))
    (fun (_,inc) -> if !verbose then Types.printC inc)
  else
    checkDec' topLevel c d

let rec addImportProc c = function
  Inter.STRUCT(inc) -> Types.union c inc
| Inter.PRL(p,q) -> addImportProc (addImportProc c p) q
| Inter.VAL(l,p) -> addImportProc (List.fold_left addImportVal c l) p
| Inter.CCODE(_,_,_,_,p) -> addImportProc c p
| Inter.CCALL(_,_,_,_,p) -> addImportProc c p
| Inter.ATOM(_,_,p) -> addImportProc c p
| Inter.NEW(_,_,p) -> addImportProc c p
| Inter.IF(_) | Inter.INPUT(_) | Inter.OUTPUT(_)
| Inter.SEND(_) | Inter.SKIP -> c

and addImportVal c = function
  Inter.DEF(_,_,_,p) -> addImportProc c p
| Inter.TUPLE(_) | Inter.STRING(_) -> c

let addImport c prog = addImportProc c prog.Inter.proc

let check imports prog =
  let c = List.fold_left addImport Types.empty imports in
  let (d,inc) = checkDec true c prog.Syntax.dec in
  {includes = prog.Syntax.includes; static = prog.Syntax.static;
   types = inc; dec = d}
