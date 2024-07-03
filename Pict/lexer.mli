(*
 * Lexer.
 *)

val create : string -> in_channel -> Lexing.lexbuf
val info : Lexing.lexbuf -> Error.info
val main : Lexing.lexbuf -> Parser.token
