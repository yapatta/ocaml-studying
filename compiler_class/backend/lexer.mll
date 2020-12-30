
(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 let line_num = ref 1
 let next_line_num = ref 1
 let add_num = ref 0
 let increment () = add_num := !add_num + 1
 let now_lex = ref ""
 let next_lex = ref ""
 let error_message () = Printf.sprintf "Syntax Error at line %d, Before lexical unit: %s\n" !line_num !now_lex
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let comment = '/' '/' [^ '\n']*

rule lexer = parse
| digit+ as num           { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NUM (int_of_string num) }
| "if"                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IF }
| "else"                  { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ELSE }
| "while"                 { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; WHILE }
| "do"                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; DO }
| "scan"                  { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SCAN }
| "sprint"                { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SPRINT }
| "iprint"                { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; IPRINT }
| "int"                   { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; INT }
| "return"                { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RETURN }
| "type"                  { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TYPE }
| "void"                  { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; VOID }
| id as text              { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ID text }
| '\"'[^'\"']*'\"' as str { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; STR str }
| '='                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; ASSIGN }
| "+="                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; INC_ASSIGN }
| "=="                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; EQ }
| "!="                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; NEQ }
| '>'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GT }
| '<'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LT }
| ">="                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; GE }
| "<="                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LE }
| '+'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; PLUS }
| "++"                    { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; INC }
| '-'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; MINUS }
| '*'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; TIMES }
| '/'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; DIV }
| '%'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; MOD }
| '^'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; POW }
| '{'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LB  }
| '}'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RB  }
| '['                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LS }
| ']'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RS }
| '('                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; LP  }
| ')'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; RP  }
| ','                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; COMMA }
| ';'                     { line_num := !next_line_num; next_line_num := !next_line_num + !add_num; add_num := 0; now_lex := !next_lex; next_lex := Lexing.lexeme lexbuf; SEMI }
| comment                 { lexer lexbuf }
| ['\n']                  { increment (); lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { lexer lexbuf }(* eat up whitespace *) 
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }

