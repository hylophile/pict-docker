let showStats = Flags.createFlag "stats" 
  "*Show compilation statistics" false

let verbose = Flags.createFlag "verbose" 
  "*Trace the progress of compilation" false

let grabTime () =
  let t = Unix.times() in
  t.Unix.tms_utime +. t.Unix.tms_stime +.
  t.Unix.tms_cutime +. t.Unix.tms_cstime

let showTime m tu =
  let su = Printf.sprintf "%.1f" tu in
  Format.print_string "  "; Format.print_string m; Format.print_string ":";
  Format.print_string (String.make (50 - (String.length m)) ' ');
  Format.print_string su; Format.print_newline()

let marks = ref([] : (string * float) list)

let mark name =
  if !verbose then (Format.print_string name; Format.print_newline());
  marks := (name, grabTime()) :: !marks

let progress name =
  if !verbose then
    (Format.print_string "# "; Format.print_string name;
     Format.print_newline())

let () = mark "initialization"

let rec showTimes nextTime = function
  (n,t) :: rest -> showTimes t rest; showTime n (nextTime -. t)
| [] -> ()

let summary () = 
  let endTime = grabTime() in
  if !showStats then begin
    showTime "Total compilation time" endTime; Format.print_newline();
    showTimes endTime (!marks); Format.print_newline()
  end
