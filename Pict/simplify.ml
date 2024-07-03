(*
 * CPS coversion from high-level Pict to core Pict.
 *)

type status = RESPONSIVE | UNKNOWN

type proc =
  PRL of proc * proc
| IF of value * proc * proc
| INPUT of value * pat * proc
| OUTPUT of status * value * value
| LET of dec * proc
| SKIP

and pat =
  RECORDp of fieldPat list
| ASp of Var.var * ty * pat
| VARp of Var.var * ty
| WILDp

and fieldPat =
  ANONp of pat
| TYp of Var.var

and dec =
  DEF of (Var.var * pat * proc) list
| INLINE of Var.var * pat * proc
| NEW of Var.var * ty
| VAL of pat * value
| SEQ of dec * dec
| RUN of proc

and ty =
  BOXED
| UNBOXED
| VAR of Var.var

and value =
  CCODEv of Ccode.info * string list * fieldVal list
| CCALLv of Ccode.info * string * fieldVal list
| COERCIONv of Ccode.coercion * value
| APPv of status * value * fieldVal list
| RECORDv of fieldVal list
| STRINGv of string
| SELECTv of value * int
| VARv of Inter.atom
| BOOLv of bool
| CHARv of char
| INTv of int
| LETv of dec * value
| IFv of value * value * value
| ABSv of pat * proc

and fieldVal =
  ANONv of value
| TYv of ty

type toplevel =
  {includes : string list;
   static : string list;
   types : Inter.atom Types.c;
   dec : dec}

type cont =
  CONT of (Inter.atom -> Inter.proc)
| CHAN of Inter.atom

let throw a = function
  (CONT f) -> f a
| (CHAN ch) -> Inter.SEND(ch,[a])

let rec simplifyPat root p = function
  RECORDp(l) -> simplifyFieldPat 0 root p l
| ASp(n,ty,pat) -> Inter.ATOM(n,root,simplifyPat root p pat)
| VARp(n,ty) -> Inter.ATOM(n,root,p)
| WILDp -> p

and simplifyFieldPat i root p = function
  [] -> p
| TYp(x) :: l -> simplifyFieldPat i root p l
| ANONp(pat) :: l ->
    let var = Var.fresh() in
    Inter.ATOM(
      var,Inter.PROJECT(root,i),
      simplifyPat (Inter.DYNAMIC var) (simplifyFieldPat (i+1) root p l) pat
    )

let rec simplifyProc = function
  PRL(p,q) ->
    Inter.PRL(simplifyProc p,simplifyProc q)
| INPUT(v,pat,p) ->
    simplifyVal v (CONT (fun v ->
      let cl = Var.fresh() and arg = Var.fresh() in
      Inter.VAL([Inter.DEF(cl,Inter.INLINABLE,[Some arg],
			   simplifyPat (Inter.DYNAMIC arg)
			   (simplifyProc p) pat)],
		Inter.INPUT(v,Inter.DYNAMIC cl,Inter.UNKNOWN))
    ))
| OUTPUT(UNKNOWN,v,av) ->
    simplifyVal v (CONT (fun v ->
      simplifyVal av (CONT (fun av -> Inter.OUTPUT(v,av,Inter.UNKNOWN)))
    ))
| OUTPUT(RESPONSIVE,v,av) ->
    simplifyVal v (CONT (fun v -> simplifyVal av (CHAN v)))
| SKIP -> Inter.SKIP
| LET(d,p) -> simplifyDec d (simplifyProc p)
| IF(v,p,q) ->
    simplifyVal v (CONT (fun v -> Inter.IF(v,simplifyProc p,simplifyProc q)))

and simplifyVal v cont = match v with
  CCODEv(ci,sl,l) ->
    simplifyFieldVal l [] (fun l ->
      (match ci.Ccode.kind with
         Ccode.EXP ->
	   (match sl with
	     [s] when
	       not(ci.Ccode.reader or ci.Ccode.writer or ci.Ccode.alloc > 0)
	     ->
	       throw (Inter.CCONST(ci,s)) cont
            | _ ->
	       let res = Var.fresh() in
	       Inter.CCODE(Some res,ci,sl,List.rev l,
			   throw (Inter.DYNAMIC res) cont))
       | Ccode.STATEMENT ->
	   Inter.CCODE(None,ci,sl,List.rev l,throw (Inter.INT 0) cont))
    )
| CCALLv(ci,s,l) ->
    simplifyFieldVal l [] (fun l ->
      (match ci.Ccode.kind with
         Ccode.EXP ->
	   let res = Var.fresh() in
	   Inter.CCALL(Some res,ci,s,List.rev l,throw (Inter.DYNAMIC res) cont)
       | Ccode.STATEMENT ->
	   Inter.CCALL(None,ci,s,List.rev l,throw (Inter.INT 0) cont))
    )
| COERCIONv(c,v) ->
    simplifyVal v (CONT (fun v -> throw (Inter.COERCION(c,v)) cont))
| RECORDv([]) ->
    throw (Inter.INT 0) cont
| RECORDv(l) ->
    simplifyFieldVal l [] (fun l ->
      let var = Var.fresh() in
      Inter.VAL([Inter.TUPLE(var,List.rev l)],throw (Inter.DYNAMIC var) cont)
    )
| APPv(s,v,al) ->
    simplifyVal v (CONT (fun v ->
      simplifyFieldVal al [] (fun l ->
	let arg = Var.fresh() in
	let body =
	  match s with
	    RESPONSIVE -> Inter.SEND(v,[Inter.DYNAMIC arg])
	  | UNKNOWN -> Inter.OUTPUT(v,Inter.DYNAMIC arg,Inter.UNKNOWN)
	in
	  match cont with
	    (CONT f) ->
	      let ch = Var.fresh() and res = Var.fresh() in
	      let a = Inter.DYNAMIC ch in
	      Inter.VAL(
		[Inter.DEF(ch,Inter.NOTINLINABLE,[Some res],
			   f (Inter.DYNAMIC res));
		 Inter.TUPLE(arg,List.rev (a :: l))],
		 body
	      )
	  | (CHAN ch) ->
	      Inter.VAL([Inter.TUPLE(arg,List.rev (ch :: l))],body)
      )
    ))
| SELECTv(v,i) ->
    simplifyVal v (CONT (fun v -> throw (Inter.PROJECT(v,i)) cont))
| STRINGv(s) ->
    let x = Var.fresh() in
    Inter.VAL([Inter.STRING(x,s)],throw (Inter.DYNAMIC x) cont)
| CHARv(x) -> throw (Inter.CHAR x) cont
| VARv(a) -> throw a cont
| BOOLv(b) -> throw (Inter.BOOL b) cont
| INTv(i) -> throw (Inter.INT i) cont
| LETv(d,v) -> simplifyDec d (simplifyVal v cont)
| ABSv(pat,p) ->
    let x = Var.fresh() and arg = Var.fresh() in
    let p = simplifyPat (Inter.DYNAMIC arg) (simplifyProc p) pat in
    Inter.VAL([Inter.DEF(x,Inter.NOTINLINABLE,[Some arg],p)],
	      throw (Inter.DYNAMIC x) cont) 
| IFv(b,v1,v2) ->
    begin
      match cont with
        (CONT f) ->
	  simplifyVal b (CONT (fun b ->
	    let ch = Var.fresh() and res = Var.fresh() in
	    let cont = CHAN(Inter.DYNAMIC ch) in
	    Inter.VAL(
	      [Inter.DEF(ch,Inter.NOTINLINABLE,[Some res],
			 f (Inter.DYNAMIC res))],
	      Inter.IF(b,simplifyVal v1 cont,simplifyVal v2 cont)
	    )
	  ))
      | (CHAN ch) ->
	  simplifyVal b (CONT (fun b ->
	    Inter.IF(b,simplifyVal v1 cont,simplifyVal v2 cont)
	  ))
    end

and simplifyFieldVal l accum cont = match l with
  [] -> cont accum
| ANONv(v) :: l ->
    simplifyVal v (CONT (fun v -> simplifyFieldVal l (v :: accum) cont))
| TYv(_) :: l ->
    simplifyFieldVal l accum cont

and simplifyDec d p = match d with
  SEQ(d1,d2) -> simplifyDec d1 (simplifyDec d2 p)
| DEF(l) -> Inter.VAL(List.map simplifyDef l,p)
| INLINE(n,pat,q) ->
    let arg = Var.fresh() in
    let q = simplifyPat (Inter.DYNAMIC arg) (simplifyProc q) pat in
    Inter.VAL([Inter.DEF(n,Inter.INLINABLE,[Some arg],q)],p)
| VAL(pat,v) -> simplifyVal v (CONT (fun v -> simplifyPat v p pat))
| NEW(n,t) -> Inter.NEW(n,Inter.REQUIRED,p)
| RUN(q) -> Inter.PRL(simplifyProc q,p)

and simplifyDef (x,pat,p) =
  let arg = Var.fresh() in
  Inter.DEF(x,Inter.NOTINLINABLE,[Some arg],
	    simplifyPat (Inter.DYNAMIC arg) (simplifyProc p) pat)

let simplify _ prog =
  {Inter.includes = prog.includes;
   Inter.static = prog.static; Inter.constants = [];
   Inter.proc = simplifyDec prog.dec (Inter.STRUCT prog.types)}
