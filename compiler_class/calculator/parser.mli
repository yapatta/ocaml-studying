type token =
  | NUM of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LP
  | RP
  | EOL

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
