(*
 * That funny old tak function that everyone plays with.
 *)

let rec tak x y z =
  if y >= x then
    z
  else
    tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
;;

print_string "tak[24,16,8] = ";
print_int (tak 24 16 8);
print_string "\n";
exit 0;;
