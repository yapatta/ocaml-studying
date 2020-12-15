(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 let line_num = ref 0
 let increment () = line_num := !line_num + 1
 let now_lex = ref ""
 let next_lex = ref ""
 let error_message () = Printf.sprintf "Syntax Error at line %d, Before lexical unit: %s\n" !line_num !now_lex
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let comment = '/' '/' [^ '\n']*

rule lexer = parse
| digit+ as num  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NUM (int_of_string num) }
| "if"                    { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IF }
| "else"                  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ELSE }
| "while"                 { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; WHILE }
| "scan"                  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SCAN }
| "sprint"                { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SPRINT }
| "iprint"                { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IPRINT }
| "int"                   { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; INT }
| "return"                { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RETURN }
| "type"                  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TYPE }
| "void"                  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; VOID }
| id as text              { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ID text }
| '\"'[^'\"']*'\"' as str { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; STR str }
| '='                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ASSIGN }
| "=="                    { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; EQ }
| "!="                    { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NEQ }
| '>'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GT }
| '<'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LT }
| ">="                    { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GE }
| "<="                    { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LE }
| '+'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; PLUS }
| '-'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; MINUS }
| '*'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TIMES }
| '/'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; DIV }
| '{'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LB  }
| '}'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RB  }
| '['                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LS }
| ']'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RS }
| '('                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LP  }
| ')'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RP  }
| ','                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; COMMA }
| ';'                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SEMI }
| comment                 { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; lexer lexbuf }
| ['\n']                  { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; increment (); lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; lexer lexbuf }(* eat up whitespace *) 
| eof                     { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; raise End_of_file }
| _                       { Printf.printf "1) now_lex: %s, next_lex: %s\n" !now_lex !next_lex; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; raise No_such_symbol }
