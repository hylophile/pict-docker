(*
 * Flags, accessible dynamically by name.
 *)

(* In this module, we use the lower-level Prim error reporting instead
   of the functions in main.mli to avoid circular linkage dependencies *)

(* (Current value, documentation, default value, currently-enabled *)
type flag = bool ref * string * bool * bool

let flags = ref (Misc.StringMap.empty : flag Misc.StringMap.t);;

let createFlagInternal name doc init switch =
  let f = ref init
in
  try
    Misc.StringMap.find name (!flags);
    Error.err("Multiple definition of flag: " ^ name)
  with Not_found ->
    (flags := Misc.StringMap.add name (f,doc,init,switch) (!flags); 
     f)

let createFlagIfEnabled name doc init switch =
  createFlagInternal name doc init switch

let createFlag name doc init =
  createFlagInternal name doc init true

let setFlag fi b name = 
  try
    let (f,_,_,enabled) = Misc.StringMap.find name (!flags) in
      if enabled then f := b
      else Error.warning ("Flag " ^ name ^ " is disabled")
  with Not_found ->
    Error.errAt fi ("Unknown flag: " ^ name)

let expert = createFlag "expert" 
  "Enable some compiler debugging features" false

let printFlag name (_, doc, init, enabled) =
  if String.get doc 0 <> '*' or !expert then begin
    Format.print_string "  ";
    Format.print_string name;
    if not enabled then Format.print_string " (disabled)";
    Format.print_string "\n    ";
    Format.print_string doc;
    Format.print_string "\n    default:";
    if init then Format.print_string "true" else Format.print_string "false";
    Format.print_newline()
  end

let printFlags () =
  Misc.StringMap.format "Internal flags:\n" "" "" printFlag (!flags)

(* ---------------------------------------------------------------------- *)

type cmd = string * (Error.info -> string list -> unit)

let cmds = ref (Misc.StringMap.empty : cmd Misc.StringMap.t);;

let createCmd name doc beh =
  try
    Misc.StringMap.find name (!cmds);
    Error.err("Multiple definition of command: " ^ name)
  with Not_found ->
    cmds := Misc.StringMap.add name (doc,beh) (!cmds)

let doCmd fi name args = 
  try
    let (_,beh) = Misc.StringMap.find name (!cmds) in beh fi args
  with Not_found ->
    Error.errAt fi ("Unknown command: " ^ name)

(* Install commands for setting and resetting flags, and for printing
   out the documentation of all flags and commands *)

let () = createCmd "set" "Set one or more flags to TRUE"
  (fun fi l -> List.iter (fun f -> setFlag fi true f) l)

let () = createCmd "reset" "Set one or more flags to FALSE"
  (fun fi l -> List.iter (fun f -> setFlag fi false f) l)

let printCmd name (doc, beh) =
  if String.get doc 0 <> '*' or !expert then begin
    Format.print_string "  ";
    Format.print_string name;
    Format.print_string "\n    ";
    Format.print_string doc;
    Format.print_newline()
  end

let printCmds () =
  Misc.StringMap.format "Internal commands:\n" "" "" printCmd (!cmds)

(* ---------------------------------------------------------------------- *)

let () = createCmd "help" "Print this usage message"
  (fun _ _ -> printCmds(); printFlags())

(* ---------------------------------------------------------------------- *)

let () = createCmd "printErrorsAsIfInFile" 
  "*Print errors as if current source were named FILE (default 'example.pi')"
  (fun _ ->
     function [s] -> Error.printErrorsAsIfInFile s
     | _ -> Error.printErrorsAsIfInFile "example.pi")
