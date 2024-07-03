(*
 * Toplevel compilation loop.
 *)

let sep =
  Flags.createFlag "sep" "Separately compile this module" false

let executeAfterCompiling =
  Flags.createFlag "execute"
    "Execute after compiling (autoset when no output file given)" false 

let lib = Flags.createFlag "lib" 
  "Automatically import the standard library lib.src" true

let debuggingInfo =
  Flags.createFlag "debuggingInfo"
  "Include debugging information in the compiled C code" false

let optC = Flags.createFlag "optC" "Optimise the generated C code" true

let keep = Flags.createFlag "keep" "Keep temporary files" false

let globalOpt = Flags.createFlag "globalOpt" "Do global optimisation" false

(*
 * Search path stuff.
 *)

let dirs = try ["."; Sys.getenv "PICTLIB"] with Not_found -> ["."];;
let searchPath = ref dirs;;

let _ = Flags.createCmd "setPath"
  "Set the import search path" (fun _ l -> searchPath := l)

let _ = Flags.createCmd "appendToPath"
  "Append some directories to the import search path"
  (fun _ l -> searchPath := l @ !searchPath)

exception BadFile of string

let find name suffix =
  let rec trypath = function
      [] -> raise (BadFile
	 ("Could not find " ^ name ^ suffix ^ " in the directories " ^
	  (List.fold_left (fun s t -> s ^ " " ^ t) "" !searchPath)))
    | (p::t) ->
      try let full = Filename.concat p name in (open_in (full ^ suffix),full)
      with Sys_error _ -> trypath t
in
  if String.length name = 0 then
    raise (BadFile "Null file name")
  else if not(Filename.is_implicit name) then
    try (open_in (name ^ suffix),name) with Sys_error m -> 
    raise (BadFile ("Could not open " ^ name ^ suffix ^ " (" ^ m ^ ")"))
  else 
    trypath !searchPath

let findFile fi name suffix =
  try find name suffix with BadFile msg -> Error.errAt fi msg

let findDep fi name suffix =
  try let (pi,full) = find name suffix in close_in pi; Some full
  with BadFile msg -> Error.warningAt fi msg; None

(*
 * Parsing.
 *)

let parseFile inFile =
  let pi =
    try open_in inFile with Sys_error m -> 
    Error.err ("Could not open " ^ inFile ^ " (" ^ m ^ ")")
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    Error.errAt (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

(*
 * Dependency generator stuff.
 *)

let dep = Flags.createFlag "dep" 
  "Generate dependencies (disables all compilation)" false

let printDep os (fi,name) =
  match findDep fi name ".pi" with
    Some full -> output_string os full; output_string os ".px "
  | None -> ()

let fileDeps os inFile =
  Stats.progress ("checking dependencies for " ^ inFile);
  let imports =
    try
      let pi = open_in inFile in
      let lexbuf = Lexer.create inFile pi in
      let result =
        try
	  (Parser.toplevel Lexer.main lexbuf).Syntax.imports
        with Parsing.Parse_error -> 
          Error.warningAt (Lexer.info lexbuf) "Parse error"; []
      in Parsing.clear_parser(); close_in pi; result 
    with Sys_error m -> 
      Error.warning ("Could not open " ^ inFile ^ " (" ^ m ^ ")"); []
  in let imports =
    if !lib then (Error.unknown,"lib") :: imports else imports
  in
    if Filename.check_suffix inFile ".pi" then
      begin
        let base = Filename.chop_suffix inFile ".pi" in
	output_string os base; output_string os " ";
	output_string os base; output_string os ".px : ";
	output_string os base; output_string os ".pi ";
	List.iter (printDep os) imports; output_string os "\n"
      end
    else
      Error.errAt Error.progName "Unknown file suffix"

(*
 * Importing separate compiled (or parsed) modules.
 *)

module Set = Misc.StringSet

let rec importAll inProgress ok files imports = function
  [] -> (ok,files,imports)
| ((fi,inFile) :: rest) ->
  if Set.mem inFile ok then
    importAll inProgress ok files imports rest
  else if Set.mem inFile inProgress then
    Error.errAt fi "Cyclic import dependency"
  else
    begin
      Stats.progress ("importing " ^ inFile);
      let (pi,full) = findFile fi inFile ".px" in
      Stats.progress (" loading " ^ full);
      let (deps,prog) = input_value pi in close_in pi;
      let (ok,files,imports) =
	importAll (Set.add inFile inProgress) ok files imports deps
      in
	importAll inProgress (Set.add inFile ok)
	(full :: files) (prog :: imports) rest
    end

let check message r s =
  match !r with
    None -> r := Some s
  | Some _ -> Error.err message

let parseArgs () =
  let inFiles = ref ([] : string list) in
  let outfile = ref (None : string option) in
  let ccFlags = ref "-Wall" in
  Arg.parse [
    "-set", Arg.String (Flags.setFlag Error.unknown true), "Set a flag";
    "-reset", Arg.String (Flags.setFlag Error.unknown false), "Reset a flag";
    "-do", Arg.String (fun c -> Flags.doCmd Error.unknown c []),
    "Execute an internal command";
    "-I", Arg.String (fun f -> Flags.doCmd Error.unknown "appendToPath" [f]),
    "Append a directory to the search path";
    "-o", Arg.String (check "Output file already specified" outfile),
    "Specify the output file";
    "-cc", Arg.String (fun s -> ccFlags := !ccFlags ^ " " ^ s),
    "Specify an argument for the C compiler"
  ] (fun s -> inFiles := s :: !inFiles) "";
  match !outfile with
    None ->
      executeAfterCompiling := true;
      (List.rev !inFiles, "./a.out", !ccFlags)
  | Some oFile -> (List.rev !inFiles, oFile, !ccFlags)

let gcc link ccFlags outFile files decls =
  let tempFile = ".PICT" ^ string_of_int(Unix.getpid()) ^ ".c" in
  let objStream = try open_out tempFile with Sys_error s ->
    Error.err ("Couldn't open " ^ tempFile ^ " for output" ^ "(" ^ s ^ ")")
  in
    (try
     Stats.mark "Printing C code";
     Ccode.printDecls objStream decls;
     close_out objStream
     with Error.Exit x -> close_out objStream; raise (Error.Exit x));
  Stats.mark "Compiling C code";
  let pictrts = Sys.getenv "PICTRTS" in
  let cmd =
    Sys.getenv "GCC" ^
    " " ^ ccFlags ^
    (if link then "" else " -c") ^
    (if !debuggingInfo then " -g" else "") ^
    (if link & not !debuggingInfo then " -Xlinker -s" else "") ^
    (if !optC & not !debuggingInfo then " -O -fomit-frame-pointer" else "") ^
    " -o " ^ outFile ^
    " -I " ^ pictrts ^
    " -idirafter " ^ Sys.getenv "X11INCLUDE" ^
    " " ^ tempFile ^
    (if link then
       (List.fold_left (fun s f -> s ^ " " ^ f ^ ".px.o") "" files) ^
       (if !debuggingInfo then " " ^ pictrts ^ "/pictLibG.a"
        else if !Codegen.profiling then " " ^ pictrts ^ "/pictLibP.a"
	else " " ^ pictrts ^ "/pictLib.a") ^
       " -lm " ^
       " -L" ^ Sys.getenv "X11LIB" ^ " -lX11 " ^
       " " ^ Sys.getenv "X11EXTRA"
     else
       "")
in
  Stats.progress cmd;
  if Sys.command cmd <> 0 then Error.err "C compilation failed";
  if !keep then
    Error.warning ("Temporary file " ^ tempFile ^ " not removed")
  else
    Sys.remove tempFile

let runit executable =
  Stats.mark "Executing";
  let cmd = executable in
  if Sys.command cmd <> 0
    then Error.err "Pict program exited with nonzero status"

let main () =
  Stats.mark "Checking args";
  let (inFiles,outFile,ccFlags) = parseArgs() in
  if !dep then
    let os = try open_out outFile with Sys_error s ->
      Error.errAt Error.progName
      ("Couldn't open " ^ outFile ^ " for output (" ^ s ^ ")")
    in
      (List.iter (fileDeps os) inFiles; close_out os)
  else
    begin
      Stats.mark "Parsing"; 
      let (inFile,prog) =
	match inFiles with
	  [inFile] -> let prog = parseFile inFile in (inFile,prog)
        | _ -> Error.err "You must specify exactly one input file"
      in
      let deps =
	if !lib then (Error.unknown,"lib") :: prog.Syntax.imports 
	else prog.Syntax.imports
      in
      Stats.mark "Importing";
      let (files,imports) =
	let (_,f,i) = importAll Set.empty Set.empty [] [] deps in
	(List.rev f,List.rev i)
      in
      if Filename.check_suffix inFile ".pi" then
	Var.setModuleName(Filename.chop_suffix inFile ".pi")
      else
	Var.setModuleName inFile;
      Stats.mark "Typechecking";
      let prog = Check.check imports prog in
      if !sep then
	begin
	  Stats.mark "Simplification";
	  let prog = Simplify.simplify imports prog in
	  Stats.mark "Optimisation";
	  let prog = Optimise.optimise imports prog in
	  let px = try open_out outFile with Sys_error s ->
	    Error.errAt Error.progName
	    ("Couldn't open " ^ outFile ^ " for output (" ^ s ^ ")")
	  in
	    Stats.mark "Exporting";
	    output_value px (deps,prog); close_out px;
	    Stats.mark "C code generation";
	    let decls = Codegen.constants imports prog in
	    gcc false ccFlags (outFile ^ ".o") files decls
	end
      else
	begin
	  Stats.mark "Simplification";
	  let prog = Simplify.simplify imports prog in
	  Stats.mark "Joining imports";
	  let prog = Inter.joinImports imports prog in
	  Stats.mark "Optimisation";
	  let prog = Optimise.optimise imports prog in
	  Stats.mark "C code generation";
	  let decls = Codegen.trans imports prog in
	  gcc true ccFlags outFile files decls;
          if !executeAfterCompiling then runit outFile
	end
    end

let () = Format.set_max_boxes 1000
let res = Printexc.catch (fun () -> try main();0 with Error.Exit x -> x) ()
let () = Stats.summary()
let () = Format.print_flush()
let () = exit res
