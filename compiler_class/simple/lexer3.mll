(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 let line_num = ref 0
 let increment () = line_num := !line_num + 1
 let next_lex = ref ""
 let error_message () = Printf.sprintf "Syntax Error at line %d, Before lexical unit: %s" !line_num !next_lex
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let comment = '/' '/' [^ '\n']*

rule lexer = parse
| digit+ as num           { next_lex := Lexing.lexeme lexbuf; NUM (int_of_string num) }
| "if"                    { next_lex := Lexing.lexeme lexbuf; IF }
| "else"                  { next_lex := Lexing.lexeme lexbuf; ELSE }
| "while"                 { next_lex := Lexing.lexeme lexbuf; WHILE }
| "scan"                  { next_lex := Lexing.lexeme lexbuf; SCAN }
| "sprint"                { next_lex := Lexing.lexeme lexbuf; SPRINT }
| "iprint"                { next_lex := Lexing.lexeme lexbuf; IPRINT }
| "int"                   { next_lex := Lexing.lexeme lexbuf; INT }
| "return"                { next_lex := Lexing.lexeme lexbuf; RETURN }
| "type"                  { next_lex := Lexing.lexeme lexbuf; TYPE }
| "void"                  { next_lex := Lexing.lexeme lexbuf; VOID }
| id as text              { next_lex := Lexing.lexeme lexbuf; ID text }
| '\"'[^'\"']*'\"' as str { next_lex := Lexing.lexeme lexbuf; STR str }
| '='                     { next_lex := Lexing.lexeme lexbuf; ASSIGN }
| "=="                    { next_lex := Lexing.lexeme lexbuf; EQ }
| "!="                    { next_lex := Lexing.lexeme lexbuf; NEQ }
| '>'                     { next_lex := Lexing.lexeme lexbuf; GT }
| '<'                     { next_lex := Lexing.lexeme lexbuf; LT }
| ">="                    { next_lex := Lexing.lexeme lexbuf; GE }
| "<="                    { next_lex := Lexing.lexeme lexbuf; LE }
| '+'                     { next_lex := Lexing.lexeme lexbuf; PLUS }
| '-'                     { next_lex := Lexing.lexeme lexbuf; MINUS }
| '*'                     { next_lex := Lexing.lexeme lexbuf; TIMES }
| '/'                     { next_lex := Lexing.lexeme lexbuf; DIV }
| '{'                     { next_lex := Lexing.lexeme lexbuf; LB  }
| '}'                     { next_lex := Lexing.lexeme lexbuf; RB  }
| '['                     { next_lex := Lexing.lexeme lexbuf; LS }
| ']'                     { next_lex := Lexing.lexeme lexbuf; RS }
| '('                     { next_lex := Lexing.lexeme lexbuf; LP  }
| ')'                     { next_lex := Lexing.lexeme lexbuf; RP  }
| ','                     { next_lex := Lexing.lexeme lexbuf; COMMA }
| ';'                     { next_lex := Lexing.lexeme lexbuf; SEMI }
| comment                 { next_lex := Lexing.lexeme lexbuf; lexer lexbuf }
| ['\n']                  { next_lex := Lexing.lexeme lexbuf; increment (); lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { next_lex := Lexing.lexeme lexbuf; lexer lexbuf }(* eat up whitespace *) 
| eof                     { next_lex := Lexing.lexeme lexbuf; raise End_of_file }
| _                       { next_lex := Lexing.lexeme lexbuf; raise No_such_symbol }
