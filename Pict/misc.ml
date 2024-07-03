
let iterateInt start finish step f res =
  if step = 0 then
    Error.bug "Misc.iterateInt"
  else if step > 0 then 
    let rec iterateUp index res =
      if index > finish then res
      else iterateUp (index + step) (f index res)
    in
      iterateUp start res
  else
    let rec iterateDown index res =
      if index < finish then res
      else iterateDown (index + step) (f index res)
    in
      iterateDown start res

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val union : 'a t -> 'a t -> 'a t
    val map : (key -> 'a -> 'b) -> 'a t -> 'b t
    val format : string -> string -> string -> (key -> 'a -> unit) -> 'a t -> unit
  end

module Make (Ord:Map.OrderedType) : (S with type key = Ord.t) =
  struct
    module M = Map.Make(Ord)
    type key = Ord.t
    type 'a t = 'a M.t
    let empty = M.empty
    let add = M.add
    let find = M.find
    let remove = M.remove
    let iter = M.iter
    let fold = M.fold
    let map f m = M.fold (fun k e m -> M.add k (f k e) m) m M.empty
    let union m1 m2 = M.fold M.add m2 m1

    let format start finish sep p m =
      let rec pr k e needSep =
	if needSep then
	  (Format.print_string sep; Format.print_cut(); p k e; true)
	else
	  (p k e; true)
    in
      Format.print_string start;
      Format.open_hovbox 0; M.fold pr m false;
      Format.print_string finish;
      Format.close_box()
  end

module StringMap =
  Make(struct
    type t = string
    let compare = compare
  end)

module IntMap =
  Make(struct
    type t = int
    let compare = (-)
  end)

let formatList start finish sep p l =
  let rec pr needSep = function
    [] -> ()
  | (h :: t) ->
    if needSep then
      (Format.print_string sep;
       Format.print_cut(); p h;
       pr true t)
    else
      (p h; pr true t)
in
  Format.print_string start;
  Format.open_hovbox 0;
  pr false l;
  Format.print_string finish;
  Format.close_box()

let printList start finish sep p os l =
  let rec pr needSep = function
    [] -> ()
  | (h :: t) ->
    if needSep then
      (output_string os sep; p os h; pr true t)
    else
      (p os h; pr true t)
in
  output_string os start; pr false l; output_string os finish

let itFold f b l =
  let rec iterate i b = function
    [] -> b
  | (x::xs) -> iterate (succ i) (f i x b) xs
in
  iterate 0 b l
