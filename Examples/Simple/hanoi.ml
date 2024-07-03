(*
 * The towers of Hanoi.
 *)

let rec transfer src dest spare n =
  if n == 0 then
    []
  else
    transfer src spare dest (n-1) @
    (src,dest) :: transfer spare dest src (n-1)
;;

print_string "Simple version: ";;
print_int (List.length(transfer 'A' 'B' 'C' 15));;
print_string "\n";;

let rec accum src dest spare n res =
  if n == 0 then
    res
  else
    accum src spare dest (n-1) ((src,dest) :: accum spare dest src (n-1) res)
;;

print_string "Accumulating version: ";;
print_int (List.length(accum 'A' 'B' 'C' 15 []));;
print_string "\n";;
exit 0;;
