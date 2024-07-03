(*
 * ML lex script to convert .src files to .pi files.
 *)

{
  type mode = SRC | TEX
  type lineMode = SRC | TEX | NONE

  let mode = ref TEX
  and lineMode = ref NONE
  and newLine = ref true

  let pr s =
    if (!mode = SRC && !lineMode = NONE) || !lineMode = SRC then
      print_string s
    else
      ()
}

rule lex = parse
  eof { }
| "\n" {
    print_string "\n"; newLine := true; lineMode := NONE; lex lexbuf
  }
| "#{@}" {
    if !newLine then (newLine := false; mode := TEX)
    else (pr "#{@}"); lex lexbuf
  }
| "#{#}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{#}"); lex lexbuf
  }
| "#{*}" {
    if !newLine then (newLine := false; mode := SRC; pr "    ")
    else (pr "#{*}"); lex lexbuf
  }
| "#@" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#@"); lex lexbuf
  }
| "#&" {
    if !newLine then (newLine := false; lineMode := TEX)
    else (pr "#&"); lex lexbuf
  }
| "##" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "##"); lex lexbuf
  }
| "###" {
    if !newLine then (newLine := false; lineMode := SRC; pr "#")
    else (pr "###"); lex lexbuf
  }
| "#*" {
    if !newLine then (newLine := false; lineMode := SRC; pr "  ")
    else (pr "#*"); lex lexbuf
  }
| _ {
    newLine := false; pr (Lexing.lexeme lexbuf); lex lexbuf
  }

{
  let () =
    if Array.length Sys.argv == 2 then
      (print_string "# line 1 \""; print_string (Array.get Sys.argv 1);
       print_string "\"\n"; lex (Lexing.from_channel stdin); exit 0)
    else
      (lex (Lexing.from_channel stdin); exit 0)
}
