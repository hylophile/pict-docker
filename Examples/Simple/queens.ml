(*
 * Count the number of solutions to the "n queens" problem.
 *)

let rec foldl l f v =
  match l with
    [] -> v
  | h :: t -> foldl t f (f h v)
;;

let sizeList l = foldl l (fun _ v -> v+1) 0;;

let nqueens size =
  let rec safe x d l =
    match l with
      [] -> true
    | q :: l ->
      x <> q & x <> q+d & x <> q-d & safe x (d+1) l

  in let rec tryPosition q b l =
    if q = 0 then l else
      if safe q 1 b then
	tryPosition (q-1) b ((q :: b) :: l)
      else
	tryPosition (q-1) b l

  in let addSolution b l =
    tryPosition size b l

  in let rec gen = function
      0 -> [[]]
    | x -> foldl (gen(x-1)) addSolution []
in
  sizeList(gen size)
;;

let rec test x =
  if x <= 10 then
    (print_string "nqueens["; print_int x; print_string "] = ";
     print_int (nqueens x); print_string "\n"; test (x+1))
  else
    exit 0
;;

test 0;;
