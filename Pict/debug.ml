(*
 * Debugging utilities
 *)

let level = ref 0

let wrapCont name f before after cont =
  Format.open_vbox 1; 
  Format.print_string " ["; Format.print_int (!level);
  Format.print_string "] "; Format.open_hvbox 0; 
  Format.print_string name; Format.print_string "? ";
  before(); Format.close_box(); Format.print_cut();
  incr level;
  let wrapped x =
    begin
      decr level;
      Format.open_hvbox 0; Format.print_string "[";
      Format.print_int (!level); Format.print_string "] ";
      Format.print_string name; Format.print_string "->";
      after x; Format.close_box(); 
      Format.close_box(); Format.print_cut();
      if !level = 0 then Format.print_newline();
      cont x
    end
in
  try (f wrapped) with e -> (decr level; Format.print_flush(); raise e)

let wrap name f before after =
  wrapCont name (fun cont -> cont(f())) before after (fun x -> x)
