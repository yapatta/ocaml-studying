(* File lexer.mll *)
{
 open Parser  
 exception No_such_symbol
 let line_num = ref 1
 let increment () = line_num := !line_num + 1
 let before_lex = ref ""
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let comment = '/' '/' [^ '\n']*


rule lexer = parse
| digit+ as num  { before_lex := num; NUM (int_of_string num) }
| "if"                    { before_lex := "if"; IF }
| "else"                  { before_lex := "else"; ELSE }
| "while"                 { before_lex := "while"; WHILE }
| "scan"                  { before_lex := "scan"; SCAN }
| "sprint"                { before_lex := "sprint"; SPRINT }
| "iprint"                { before_lex := "iprint"; IPRINT }
| "int"                   { before_lex := "int"; INT }
| "return"                { before_lex := "return"; RETURN }
| "type"                  { before_lex := "type"; TYPE }
| "void"                  { before_lex := "void"; VOID }
| id as text              { before_lex := text; ID text }
| '\"'[^'\"']*'\"' as str { before_lex := str; STR str }
| '='                     { before_lex := "="; ASSIGN }
| "=="                    { before_lex := "=="; EQ }
| "!="                    { before_lex := "!="; NEQ }
| '>'                     { before_lex := ">"; GT }
| '<'                     { before_lex := "<"; LT }
| ">="                    { before_lex := ">="; GE }
| "<="                    { before_lex := "<="; LE }
| '+'                     { before_lex := "+"; PLUS }
| '-'                     { before_lex := "-"; MINUS }
| '*'                     { before_lex := "*"; TIMES }
| '/'                     { before_lex := "/"; DIV }
| '{'                     { before_lex := "{"; LB  }
| '}'                     { before_lex := "}"; RB  }
| '['                     { before_lex := "["; LS }
| ']'                     { before_lex := "]"; RS }
| '('                     { before_lex := "("; LP }
| ')'                     { before_lex := ")"; RP }
| ','                     { before_lex := ","; COMMA }
| ';'                     { before_lex := ";"; SEMI }
| comment                 { lexer lexbuf }
| ['\n']                  { before_lex := "\\n"; increment (); lexer lexbuf }(* eat up whitespace *) 
| [' ' '\t']              { before_lex := " "; lexer lexbuf }(* eat up whitespace *) 
| eof                     { raise End_of_file }
| _                       { raise No_such_symbol }
