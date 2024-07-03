(*
 * Types and simple utilties (such as printing).
 *)

let debug = Flags.createFlag "TYPES"
  "*Print low-level debugging information in the types module" false

let exactT = Flags.createFlag "exactTypes"
  "*Show complete internal representation of types" false

let printKinds = Flags.createFlag "printKinds"
  "*Print the kinds of all (non-Type) types" false

type tag = Var.var
let freshTag = Var.fresh
let printTag = Var.format
module Map = Var.Map

type t =
  TOP
| BOT
| TVAR of Kind.kind * string * tag
| REC of Kind.kind * string * tag * t
| CON of Kind.kind * (string * tag * Kind.polarity * Kind.kind) list * t
| TAPP of Kind.kind * t * t list
| RECORD of field list  
| RCHAN of t
| OCHAN of t
| ICHAN of t
| CHAN of t
| STRING
| CHAR
| BOOL
| INT

and field =
  ANON of t
| LABELED of string * t
| TY of string * tag * Kind.kind * constr

and constr =
  IN of Kind.polarity
| LT of t
| EQ of t

type 'a c =
  {types: tag Misc.StringMap.t;        (* Maps type variables to unique tags *)
   constr: (Kind.kind * constr) Map.t; (* Maps tags to constraints *)  
   vars: (t * 'a) Misc.StringMap.t} (* Maps identifiers to types, plus an 'a *)

let mkUnit = RECORD []

(*
 * Creation functions for tagged things
 *)

let empty =
  {types=Misc.StringMap.empty; constr=Map.empty; vars=Misc.StringMap.empty}

let abind c n tag =
  {types=Misc.StringMap.add n tag c.types; constr=c.constr; vars=c.vars}

let constr c tag k constr =
  {types=c.types; constr=Map.add tag (k,constr) c.constr; vars=c.vars}

let xbind n t x c =
  {types=c.types; constr=c.constr; vars=Misc.StringMap.add n (t,x) c.vars}

let union c1 c2 =
  {types=Misc.StringMap.union c1.types c2.types;
   constr=Map.union c1.constr c2.constr;
   vars=Misc.StringMap.union c1.vars c2.vars}

(*** Find the Kind of a type. ***)

let kindOf = function
  TVAR(k,_,_) -> k
| CON(k,_,_) -> k
| TAPP(k,_,_) -> k
| REC(k,_,_,_) -> k
| TOP | BOT | RECORD(_) | RCHAN(_)
| OCHAN(_) | ICHAN(_) | CHAN(_) | STRING
| CHAR | BOOL | INT -> Kind.TYPE

(*** Simple formatting utilities. ***)

let openParen () = Format.print_string "("; Format.open_hvbox 2
let closeParen () = Format.print_string ")"; Format.close_box()

(*** Pretty print a type. ***)

let ppTag tag =
  Format.print_string "("; Var.format tag; Format.print_string ")"

let ppK = function
  Kind.TYPE -> ()
| k -> Format.print_string ":"; Kind.print k

let ppKindedArg (n,tag,p,k) =
  Format.print_string n; if !exactT then ppTag tag; 
  match (p,k) with
    (Kind.PN,Kind.TYPE) -> ()
  | (p,k) ->
      Format.print_string ":"; Kind.printPolarity p;
      Format.print_space(); Kind.print k

let ppLevel i =
  Format.print_string "+"; Format.print_int i

let rec ppT = function
  TVAR(_,n,tag) ->
  Format.print_string n; if !exactT then ppTag tag
| REC(k,n,tag,t) ->
    Format.print_string "(rec ";
    Format.print_string n; if !exactT then ppTag tag; ppK k;
    Format.print_string "="; ppT t; Format.print_string ")"
| CON(_,l,t) -> 
    Misc.formatList "\(" ")=" " " ppKindedArg l; 
    Format.print_space(); ppT t
| TAPP(_,t,l) ->
    openParen(); ppT t; Format.print_space();
    Misc.formatList "" "" " " ppT l; closeParen()
| TOP ->
    Format.print_string "Top"
| BOT ->
    Format.print_string "Bot"
| RECORD(l) ->
    Misc.formatList "[" "]" " " ppF l
| RCHAN(t) -> Format.print_string "/"; ppT t
| CHAN(t) -> Format.print_string "^"; ppT t
| OCHAN(t) -> Format.print_string "!"; ppT t
| ICHAN(t) -> Format.print_string "?"; ppT t
| STRING -> Format.print_string "String"
| CHAR -> Format.print_string "Char"
| BOOL -> Format.print_string "Bool"
| INT -> Format.print_string "Int"

and ppF = function
  ANON(t) -> ppT t
| LABELED(n,t) -> Format.print_string n; Format.print_string " = "; ppT t
| TY(n,tag,k,con) ->
  Format.print_string "#"; Format.print_string n;
  if !exactT then ppTag tag; ppCon (k,con)

and ppCon = function 
  (_,LT(TOP)) -> ()
| (_,LT(t)) -> Format.print_string "<"; ppT t
| (Kind.TYPE,IN Kind.PN) -> ()
| (k,IN p) ->
    Format.print_string ":"; Kind.printPolarity p;
    Format.print_string " "; Kind.print k
| (_,EQ(t)) -> Format.print_string "="; ppT t

let printCon k con = ppCon (k,con)

let printT t =
  let k = kindOf t in
  if !printKinds & k <> Kind.TYPE then
    (Format.print_string " : "; Kind.print k);
  Format.open_hvbox 2; ppT t; Format.close_box()

let displayT s = Format.print_break 1 3; printT s

let formatVarEntry n (t,_) =
  Format.open_hvbox 2; Format.print_string n; Format.print_string " : ";
  Format.print_cut(); printT t; Format.close_box();
  Format.print_newline()

let formatTyEntry c n tag =
  Format.open_hvbox 2; Format.print_string n;
  if !exactT then ppTag tag; ppCon (Map.find tag c.constr); 
  Format.close_box(); Format.print_newline()

let printC c =
  Format.open_hovbox 0;
    Misc.StringMap.iter (formatTyEntry c) c.types;
    Misc.StringMap.iter formatVarEntry c.vars;
  Format.close_box()

(*** Iterate over all value bindings in a context. ***)

let mapBindings f c =
  {types=c.types; constr=c.constr;
   vars=Misc.StringMap.map (fun _ x -> f x) c.vars}
let allBindings f c =
  Misc.StringMap.iter (fun _ x -> f x) c.vars

let lookupTyVar c tag =
  try
    Map.find tag c.constr 
  with Not_found ->
    Error.bugf (fun ()->
      Format.print_string "lookupTyVar: tag: "; Var.format tag
    )

(*
 * Substitution
 *)

let rec sub env = function
  TOP -> TOP
| BOT -> BOT
| TVAR(_,_,tag) as s -> (try Map.find tag env with Not_found -> s)
| REC(k,n,tag,s) -> 
    let newTag = Var.fresh() in let v = TVAR(k,n,newTag) in
    REC(k,n,newTag,sub (Map.add tag v env) s)
| CON(k,l,t) -> 
    let (l,env) =
      List.fold_left (fun (l,env) (n,tag,p,k) ->
	let newTag = Var.fresh() in let v = TVAR(k,n,newTag) in
        ((n,newTag,p,k) :: l, Map.add tag v env)
      ) ([],env) l
    in
      CON(k,List.rev l,sub env t)
| TAPP(k,t,a) -> TAPP(k,sub env t,List.map (sub env) a)
| RECORD(l) -> RECORD(List.rev(snd(List.fold_left subField (env,[]) l)))
| RCHAN(t) -> RCHAN(sub env t)
| OCHAN(t) -> OCHAN(sub env t)
| ICHAN(t) -> ICHAN(sub env t)
| CHAN(t) -> CHAN(sub env t)
| STRING -> STRING
| CHAR -> CHAR
| BOOL -> BOOL
| INT -> INT

and subField (env,l) = function
  ANON(t) -> (env,ANON(sub env t) :: l)
| LABELED(n,t) -> (env,LABELED(n,sub env t) :: l)
| TY(n,tag,k,LT t) -> 
    let f = Var.fresh() in let tv = TVAR(k,n,f) in
    (Map.add tag tv env,TY(n,f,k,LT(sub env t)) :: l)
| TY(n,tag,k,IN p) -> 
    let f = Var.fresh() in let tv = TVAR(k,n,f) in
    (Map.add tag tv env,TY(n,f,k,IN p) :: l)
| TY(n,tag,k,EQ t) -> 
    let f = Var.fresh() in let tv = TVAR(k,n,f) in
    (Map.add tag tv env,TY(n,f,k,EQ(sub env t)) :: l)

let subst s env =
  if !debug then
    Debug.wrap "subst"
    (fun () -> sub env s)
    (fun () ->
       Format.print_string "type: "; printT s;
       Format.print_space();
       Map.format " " "" "," (fun tag t ->
         Var.format tag; Format.print_string " => "; printT t
       ) env
    )
    (fun res -> printT res)
  else
    sub env s

let renameBoundVars s =
  if !debug then
    Debug.wrap "renameBoundVars" (fun () -> sub Map.empty s)
    (fun () -> printT s) (fun res -> printT res)
  else
    sub Map.empty s

(*
 * Exposes a type (by exposing type equalities and expanding out
 * type constructor applications) until the type is in head normal form.
 *)

let expose c s =
  let rec exp = function
    TVAR(_,_,tag) as s ->
     (match lookupTyVar c tag with
        (_,LT _) -> s
      | (_,EQ t) -> exp (renameBoundVars t)
      | (_,IN _) -> s)
  | TAPP(_,s,tl) as app -> 
     (match exp s with
        CON(_,l,t) ->
          let sub =
            List.fold_left2 (fun sub (_,tag,_,_) arg ->
              Map.add tag arg sub
            ) Map.empty l tl
          in
            subst t sub
      | _ -> app)
  | s -> s
in
  if !debug then
    Debug.wrap "expose"
    (fun () -> exp s)
    (fun () -> printT s)
    (fun s -> printT s)
  else
    exp s

(*
 * Promote a type variable to its least (proper) upper bound.
 *)

let promote c tag = 
  match lookupTyVar c tag with
      (_, LT TOP) -> None
    | (_, LT t) -> Some(t)
    | (_, IN _) -> None
    | (_, EQ(_)) -> 
        Error.bugf (fun ()->
          Format.print_string "promote: tag: "; Var.format tag)

(*
 * Promotes a type (by exposing type equalities, promoting
 * variables in head position, and expanding out type constructor
 * applications) until the type is in head normal form.
 *)

let basis c s =
  let rec loop s = match expose c s with
      TVAR(_,_,tag) -> 
        (match promote c tag with None -> s | Some t -> loop t)
    | s -> s
  in 
    if !debug then
      Debug.wrap "basis"
      (fun () -> loop s)
      (fun () -> printT s)
      (fun s -> printT s)
    else
      loop s

(* 
 * Destructors and checkers
 *)

let getCon fi c s = basis c s (*HACK*)

let getRec fi c s =
  match basis c s with
    REC(k,n,tag,t) as r -> subst t (Map.add tag r Map.empty)
  | TAPP(_,_,_) -> Error.bug "Types.getRec: not implemented"
  | _ -> Error.errfAt fi (fun() ->
    Format.print_string "Expected recursive type but found"; displayT s)

let getFields fi c t =
  match basis c t with
    RECORD l -> l
  | _ -> Error.errfAt fi (fun () ->
     Format.print_string "Expected a record type but found "; displayT t)
  
let getIChan fi c t =
  match basis c t with
    CHAN t -> t
  | ICHAN t -> t
  | _ -> Error.errfAt fi (fun () ->
     Format.print_string "Expected an input-channel type but found ";
     displayT t)
  
let getOChan fi c t =
  match basis c t with
    CHAN t -> t
  | OCHAN t -> t
  | RCHAN t -> t
  | _ -> Error.errfAt fi (fun () ->
     Format.print_string "Expected an output-channel type but found ";
     displayT t)

let exposeCon fi c s = expose c s

let exposeRec fi c s =
  match expose c s with
    REC(k,n,tag,t) as r -> subst t (Map.add tag r Map.empty)
  | TAPP(_,_,_) -> Error.bug "Types.exposeRec: not implemented"
  | _ -> Error.errfAt fi (fun() ->
    Format.print_string "Expected recursive type but found"; displayT s)

let exposeFields fi c t =
  match expose c t with
    RECORD l -> l
  | _ -> Error.errfAt fi (fun () ->
     Format.print_string "Expected a record type but found "; displayT t)
  
let exposeORChan fi c t =
  match expose c t with
    OCHAN t -> t
  | RCHAN t -> t
  | _ -> Error.errfAt fi (fun () ->
     Format.print_string "Expected an output-channel type but found ";
     displayT t)

let isChan c t =
  match expose c t with CHAN _ -> true | _ -> false

let isInt c t =
  match expose c t with INT -> true | _ -> false

let isBool c t =
  match expose c t with BOOL -> true | _ -> false

let leqRChan c t =
  match basis c t with RCHAN _ -> true | _ -> false

let leqInt c t =
  match basis c t with INT -> true | _ -> false

let leqBool c t =
  match basis c t with BOOL -> true | _ -> false

let leqString c t =
  match basis c t with STRING -> true | _ -> false

let typeOfX fi n c =
  try Misc.StringMap.find n c.vars with Not_found ->
  Error.errfAt fi (fun () ->
    Format.print_string "Unbound name: "; Format.print_string n
  )

let isBoundVar c n =
  try Misc.StringMap.find n c.vars; true with Not_found -> false

let isBoundTyVar c n =
  try Misc.StringMap.find n c.types; true with Not_found -> false

let getFieldType fi c t n =
  match basis c t with
    RECORD(l) ->
      (let rec loop i = function 
         ANON(_) :: rest -> loop (i+1) rest
       | LABELED(fn,ft) :: rest -> if n = fn then (ft,i) else loop (i+1) rest
       | TY(_) :: rest -> loop i rest
       | [] -> 
          Error.errfAt fi (fun () ->
            Format.print_string "label "; Format.print_string n;
	    Format.print_string " missing from type ";
            displayT t)
       in loop 0 l)
  | _ -> 
    Error.errfAt fi (fun () -> 
      Format.print_string "expected a record type but found:"; displayT t
    )
