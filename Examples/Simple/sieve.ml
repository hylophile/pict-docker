(*
 * The sieve of Eratosthenes
 *)

let rec interval min max =
  if min > max then [] else min :: interval (min+1) max
;;

let rec filter p = function
  [] -> []
| hd :: tl -> if p hd then hd :: filter p tl else filter p tl
;;

let sieve max =
  let rec again = function
    [] -> []
  | n :: tl as l ->
    if n*n > max then l else n :: again (filter (fun x -> (x mod n) <> 0) tl)
in
  again (interval 2 max)
;;

let print idx x =
  if (idx mod 10) == 9 then
    (print_int x; print_string "\n"; idx+1)
  else
    (print_int x; print_string " "; idx+1)
;;

List.fold_left print 0 (sieve 4000);;
exit 0;;
