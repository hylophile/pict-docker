(*
 * Command line parsing and error printing.
 *)

exception Exit of int

type info = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let unknown = UNKNOWN
let progName = FI(Array.get Sys.argv 0, 0, 0)
let create f l c = FI(f, l, c)

let errf f = 
  Format.print_flush();
  Format.set_formatter_out_channel stderr;
  Format.open_hvbox 0; f();
  Format.print_newline(); 
  raise (Exit 1)

(* Special undocumented feature, so that error messages appearing in
   the documentation can be printed without hideous long automounter 
   pathnames *)
let fakeFileName = ref("")
let printErrorsAsIfInFile s = fakeFileName := s

let printInfo = function
  FI(f,l,c) ->
    Format.print_string (if !fakeFileName = "" then f else !fakeFileName); 
    Format.print_string ":"; 
    Format.print_int l; Format.print_string "."; 
    Format.print_int c; Format.print_string ":"
| UNKNOWN ->
    Format.print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; Format.print_space(); f())

let err s = errf (fun()-> Format.print_string s)

let errAt fi s = errfAt fi (fun()-> Format.print_string s)

let bug s = err ("Compiler bug: " ^ s)

let bugf f = errf (fun()->
  Format.print_string "Compiler bug: "; Format.print_space(); f()
)

let bugfAt fi f = errfAt fi (fun()->
  Format.print_string "Compiler bug: "; Format.print_space(); f()
)

let bugAt fi s = errAt fi ("Compiler bug: " ^ s)

let warning s =
  Format.print_string "Warning: "; Format.print_string s;
  Format.print_newline()

let warningAt fi s =
  printInfo fi; Format.print_string " Warning: ";
  Format.print_string s; Format.print_newline()
