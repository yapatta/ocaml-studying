
(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 let line_num = ref 0
 let increment () = line_num := !line_num + 1
 let now_lex = ref ""
 let next_lex = ref ""
 let print_error_message () = Printf.printf "Syntax Error at line %d, Before lexical unit: %s\n" !line_num !now_lex
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let comment = '/' '/' [^ '\n']*

rule lexer = parse
| digit+ as num           { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NUM (int_of_string num) }
| "if"                    { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IF }
| "else"                  { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ELSE }
| "while"                 { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; WHILE }
| "scan"                  { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SCAN }
| "sprint"                { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SPRINT }
| "iprint"                { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IPRINT }
| "int"                   { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; INT }
| "return"                { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RETURN }
| "type"                  { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TYPE }
| "void"                  { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; VOID }
| id as text              { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ID text }
| '\"'[^'\"']*'\"' as str { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; STR str }
| '='                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ASSIGN }
| "=="                    { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; EQ }
| "!="                    { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NEQ }
| '>'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GT }
| '<'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LT }
| ">="                    { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GE }
| "<="                    { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LE }
| '+'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; PLUS }
| '-'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; MINUS }
| '*'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TIMES }
| '/'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; DIV }
| '{'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LB  }
| '}'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RB  }
| '['                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LS }
| ']'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RS }
| '('                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LP  }
| ')'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RP  }
| ','                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; COMMA }
| ';'                     { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SEMI }
| comment                 { now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; lexer lexbuf }
| ['\n']                  { increment (); lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { lexer lexbuf }(* eat up whitespace *) 
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }
