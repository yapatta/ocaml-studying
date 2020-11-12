(* File lexer.mll *)
{
    open Parser
    exception Eof
}

rule token = parse
    ['0'-'9']+ as vl { NUM (int_of_string(vl)) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DIV }
  | '('            { LP }
  | ')'            { RP }
  | [' ' '\t']     { token lexbuf } (* skip blanks *)
  | ['\n' ]        { EOL }
  | eof            { raise Eof }
