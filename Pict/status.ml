(*
 * Channel status inference.
 *)

open Inter

let inferChannelStatus =
  Flags.createFlag "inferChannelStatus"
  "*Try and statically infer non-replicated channel statuses" true

let inferReplicatedStatus =
  Flags.createFlag "inferReplicatedStatus"
  "*Try and statically infer replicated channel statuses" true

let replicated = (Hashtbl.create 1000 : (Var.var,unit) Hashtbl.t)

let addDef ch =
  if !inferReplicatedStatus then Hashtbl.add replicated ch () else ()

let varInfo ch s =
  try Hashtbl.find replicated ch; REPLICATED with Not_found ->
  try Var.Map.find ch s with Not_found -> UNKNOWN

let scanVal = function
  DEF(x,_,_,_) -> addDef x
| TUPLE(_) | STRING(_) -> ()

let rec optProc s = function
  PRL(p,q) ->
    let (p,s) = optProc s p in
    let (q,s) = optProc s q in
    (PRL(p,q),s)
| NEW(x,i,p) ->
    if !inferChannelStatus then
      let (p,s) = optProc (Var.Map.add x EMPTY s) p in
      match varInfo x s with
        ONEREADER -> (NEW(x,NOTREQUIRED,p),s)
      | ONEWRITER -> (NEW(x,NOTREQUIRED,p),s)
      | _ -> (NEW(x,REQUIRED,p),s)
    else
      let (p,s) = optProc s p in (NEW(x,i,p),s)
| VAL(l,p) ->
    let l = List.iter scanVal l; List.map optVal l in
    let (p,s) = optProc s p in (VAL(l,p),s)
| CCODE(x,ci,sl,l,p) ->
    let (p,sp) = optProc s p in (CCODE(x,ci,sl,l,p),s)
| CCALL(x,ci,c,l,p) ->
    let (p,sp) = optProc s p in (CCALL(x,ci,c,l,p),s)
| ATOM(x,a,p) ->
    let (p,sp) = optProc s p in (ATOM(x,a,p),s)
| IF(a,p,q) ->
    let (p,sp) = optProc s p in let (q,sq) = optProc s q in
    (IF(a,p,q),Var.Map.map (fun x ip -> unionStatus (varInfo x sq) ip) sp)
| STRUCT(inc) ->
    (STRUCT inc,Var.Map.empty)
| SKIP ->
    (SKIP,s)
| OUTPUT(DYNAMIC x,y,o) ->
    let i = interStatus o (varInfo x s) in
    (match i with
      EMPTY ->
        (OUTPUT(DYNAMIC x,y,i),Var.Map.add x ONEWRITER s)
    | REPLICATED ->
        (SEND(DYNAMIC x,[y]),s)
    | ONEREADER ->
        (OUTPUT(DYNAMIC x,y,i),Var.Map.add x EMPTY s)
    | ONEWRITER ->
        (OUTPUT(DYNAMIC x,y,i),Var.Map.add x KNOWN s)
    | KNOWN ->
        (OUTPUT(DYNAMIC x,y,i),s)
    | UNKNOWN ->
        (OUTPUT(DYNAMIC x,y,i),Var.Map.empty))
| INPUT(DYNAMIC x,y,o) ->
    let i = interStatus o (varInfo x s) in
    (match i with
      EMPTY ->
        (INPUT(DYNAMIC x,y,i),Var.Map.add x ONEREADER s)
    | REPLICATED ->
	Error.bug "Status.infer: impossible channel status"
    | ONEREADER ->
	(INPUT(DYNAMIC x,y,i),Var.Map.add x KNOWN s)
    | ONEWRITER ->
	(INPUT(DYNAMIC x,y,i),Var.Map.add x EMPTY s)
    | KNOWN -> 
        (INPUT(DYNAMIC x,y,i),Var.Map.add x KNOWN s)
    | UNKNOWN -> 
        (INPUT(DYNAMIC x,y,i),Var.Map.empty))
| OUTPUT(_,_,_) as p -> (p,Var.Map.empty)
| INPUT(_,_,_) as p -> (p,Var.Map.empty)
| SEND(_,_) as p -> (p,s)

and optVal v = match v with
  DEF(x,i,args,p) -> DEF(x,i,args,fst(optProc Var.Map.empty p))
| TUPLE(_) | STRING(_) -> v

let infer _ prog =
  let l = List.iter scanVal prog.constants; List.map optVal prog.constants in
  {includes = prog.includes; static = prog.static; constants = l;
   proc = fst(optProc Var.Map.empty prog.proc)}
