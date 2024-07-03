(*
 * ML lex script to convert .src files to .tex files.
 *)

{
  let pr = print_string

  type mode = TEX | SRC | BOTH
  type lineMode = TEX | SRC | BOTH | CHARS | NONE

  let mode = ref TEX
  and lineMode = ref NONE
  and newLine = ref true
  and needsVerbatim = ref false
  and inVerbatim = ref false

  let shouldPrint () =
    (!lineMode <> SRC && !lineMode <> NONE) ||
    (!lineMode = NONE && !mode <> SRC)

  let inVerb () = needsVerbatim := true
  let outVerb () = needsVerbatim := false

  let checkVerb () =
    if !inVerbatim then
      if !needsVerbatim then ()
      else (inVerbatim := false; pr "\\end{pictsource}\n")
    else
      if !needsVerbatim then (inVerbatim := true; pr "\\begin{pictsource}\n")
      else ()

  let pc = function
    '@'	->
      if !lineMode = CHARS then
	(lineMode := TEX; pr "}")
      else if !lineMode = TEX || (!lineMode = NONE && !mode = TEX) then
	(lineMode := CHARS; pr "\\ttbox{")
      else if shouldPrint() then
	(checkVerb(); pr "@")
      else
	()
  | '\n' ->
      checkVerb(); 
      if shouldPrint() then print_char '\n'; 
      newLine := true; lineMode := NONE;
      if !mode <> BOTH then outVerb();
  | c ->
      if !lineMode = CHARS then
	match c with
	  ' ' -> pr "\\ "
	| '_' -> pr "{\\char'137}"
	| '#' -> pr "{\\char'43}"
	| '$' -> pr "{\\char'44}"
	| '%' -> pr "{\\char'45}"
	| '&' -> pr "{\\char'46}"
	| '~' -> pr "{\\char'176}"
	| '^' -> pr "{\\char'136}"
	| '\\' -> pr "{\\char'134}"
	| '{' -> pr "{\\char'173}"
	| '}' -> pr "{\\char'175}"
	| _ -> print_char c
      else if shouldPrint() then
	(checkVerb(); print_char c)
      else
	()
}

rule lex = parse
  eof { }
| "#{@}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := TEX; outVerb())
    else (pc '#'; pc '{'; pc '@'; pc '}'; pc '\n'); lex lexbuf
  }
| "#{#}" {
    if !newLine then (newLine := false; mode := SRC)
    else (pc '#'; pc '{'; pc '#'; pc '}'); lex lexbuf
  }
| "#{*}" ' '* '\n' {
    if !newLine then (newLine:=true; mode := BOTH; inVerb())
    else (pc '#'; pc '{'; pc '*'; pc '}'; pc '\n'); lex lexbuf
  }
| "#@" {
    if !newLine then (newLine := false; lineMode := TEX; outVerb())
    else (pc '#'; pc '@'); lex lexbuf
  }
| "#&" {
    if !newLine then (newLine := false; lineMode := BOTH; inVerb())
    else (pc '#'; pc '&'); lex lexbuf
  }
| "##" {
    if !newLine then (newLine := false; lineMode := SRC)
    else (pc '#'; pc '#'); lex lexbuf
  }
| "#*" {
    if !newLine then (newLine := false; lineMode := BOTH; inVerb())
    else (pc '#'; pc '*'); lex lexbuf
  }
| "@@" {
    if !lineMode = CHARS then print_char '@' else (pc '@'; pc '@'); lex lexbuf
  }
| "@@@" {
    if !lineMode = CHARS then
      (print_char '@'; pc '@'; lex lexbuf)
    else if !lineMode = TEX || (!lineMode = NONE && !mode = TEX) then
      (lineMode := CHARS; pr "\\ttbox{@"; lex lexbuf)
    else
      (pc '@'; pc '@'; pc '@'; lex lexbuf)
  }
| '_'['a'-'z' 'A'-'Z' '0'-'9']+ {
    if !lineMode = CHARS then
      (let s = Lexing.lexeme lexbuf in
       pr "}_{"; pr (String.sub s 1 (String.length s - 1));
       pr "}\\ttbox{"; lex lexbuf)
    else if shouldPrint() then
      (checkVerb(); pr (Lexing.lexeme lexbuf); lex lexbuf)
    else
      lex lexbuf
  }
| _ {
    newLine := false; pc (Lexing.lexeme_char lexbuf 0); lex lexbuf
  }

{
  let () =
    (pr "%% AUTOMATICALLY GENERATED: DO NOT MODIFY!\n";
     lex (Lexing.from_channel stdin);
     outVerb(); checkVerb(); exit 0)
}
