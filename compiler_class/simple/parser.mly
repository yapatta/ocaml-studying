%{

open Printf
open Ast
open Lexer
exception Parse_error of string
let error_message () = Printf.sprintf "At line %d, Before lexical unit: %s" !Lexer.line_num !Lexer.before_lex

%}

/* File parser.mly */
%token <int> NUM
%token <string> STR ID
%token INT IF WHILE SPRINT IPRINT SCAN EQ NEQ GT LT GE LE ELSE RETURN NEW
%token PLUS MINUS TIMES DIV LB RB LS RS LP RP ASSIGN SEMI COMMA TYPE VOID
%type <Ast.stmt> prog


%nonassoc GT LT EQ NEQ GE LE
%left PLUS MINUS         /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS      /* highest precedence */


%start prog           /* the entry point */

%%

prog : stmt  {  $1  }
     ;

ty   : INT { IntTyp }
     | INT LS NUM RS { ArrayTyp ($3, IntTyp) }
     | ID	     { NameTyp $1 }
     | INT LS error RS { raise (Parse_error (error_message ())) }
     ;

decs : decs dec { $1@$2 }
     |          { [] }
     ;

dec  : ty ids SEMI   { List.map (fun x -> VarDec ($1,x)) $2 }
     | TYPE ID ASSIGN ty SEMI { [TypeDec ($2,$4)] }
     | ty ID LP fargs_opt RP block  { [FuncDec($2, $4, $1, $6)] }
     | VOID ID LP fargs_opt RP block  { [FuncDec($2, $4, VoidTyp, $6)] }
     | ty ID LP error RP block  { raise (Parse_error (error_message ())) }
     | VOID ID LP error RP block  { raise (Parse_error (error_message ())) }
     ; 

ids  : ids COMMA ID    { $1@[$3] }
     | ID              { [$1]  }
     ;

fargs_opt : /* empty */ { [] }
     | fargs            { $1 }
     ;
     
fargs: fargs COMMA ty ID     { $1@[($3,$4)] }
     | ty ID                 { [($1,$2)] }
     ;

stmts: stmts stmt  { $1@[$2] }
     | stmt        { [$1] }
     ;

stmt : ID ASSIGN expr SEMI    { Assign (Var $1, $3) }
     | ID LS expr RS ASSIGN expr SEMI  { Assign (IndexedVar (Var $1, $3), $6) }
     | IF LP cond RP stmt     { If ($3, $5, None) }
     | IF LP cond RP stmt ELSE stmt 
                              { If ($3, $5, Some $7) }
     | WHILE LP cond RP stmt  { While ($3, $5) }
     | SPRINT LP STR RP SEMI  { CallProc ("sprint", [StrExp $3]) }
     | IPRINT LP expr RP SEMI { CallProc ("iprint", [$3]) }
     | SCAN LP ID RP SEMI  { CallProc ("scan", [VarExp (Var $3)]) }
     | NEW LP ID RP SEMI   { CallProc ("new", [ VarExp (Var $3)]) }
     | ID LP aargs_opt RP SEMI  { CallProc ($1, $3) }
     | RETURN expr SEMI    { CallProc ("return", [$2]) }
     | block { $1 }
     | SEMI { NilStmt }
     | ID ASSIGN error SEMI    { raise (Parse_error (error_message ())) }
     | ID LS error RS ASSIGN expr SEMI { raise (Parse_error (error_message ())) }
     | IF LP error RP stmt     { raise (Parse_error (error_message ())) }
     | IF LP error RP stmt ELSE stmt 
                              { raise (Parse_error (error_message ())) }
     ;

aargs_opt: /* empty */     { [] }
        | aargs            { $1 }
        ;

aargs : aargs COMMA expr  { $1@[$3] }
      | expr               { [$1] }
      | error COMMA expr  { raise (Parse_error (error_message ())) }
      ;

block: LB decs stmts RB  { Block ($2, $3) }
     ;

expr : NUM { IntExp $1  }
     | ID { VarExp (Var $1) }
     | ID LP aargs_opt RP { CallFunc ($1, $3) } 
     | ID LS expr RS  { VarExp (IndexedVar (Var $1, $3)) }
     | expr PLUS expr { CallFunc ("+", [$1; $3]) }
     | expr MINUS expr { CallFunc ("-", [$1; $3]) }
     | expr TIMES expr { CallFunc ("*", [$1; $3]) }
     | expr DIV expr { CallFunc ("/", [$1; $3]) }
     | MINUS expr %prec UMINUS { CallFunc("!", [$2]) }
     | LP expr RP  { $2 }
     | ID LP error RP { raise (Parse_error (error_message ())) } 
     | ID LS error RS { raise (Parse_error (error_message ())) }
     | error PLUS expr { raise (Parse_error (error_message ())) } 
     | error MINUS expr { raise (Parse_error (error_message ())) } 
     | error TIMES expr { raise (Parse_error (error_message ())) } 
     | error DIV expr { raise (Parse_error (error_message ())) } 
     | MINUS error %prec UMINUS { raise (Parse_error (error_message ())) }
     | LP error RP { raise (Parse_error (error_message ())) }
     ;

cond : expr EQ expr  { CallFunc ("==", [$1; $3]) }
     | expr NEQ expr { CallFunc ("!=", [$1; $3]) }
     | expr GT expr  { CallFunc (">", [$1; $3]) }
     | expr LT expr  { CallFunc ("<", [$1; $3]) }
     | expr GE expr  { CallFunc (">=", [$1; $3]) }
     | expr LE expr  { CallFunc ("<=", [$1; $3]) }
     | error EQ expr { raise (Parse_error (error_message ())) }
     | error NEQ expr { raise (Parse_error (error_message ())) }
     | error GT expr { raise (Parse_error (error_message ())) }
     | error LT expr { raise (Parse_error (error_message ())) }
     | error GE expr { raise (Parse_error (error_message ())) }
     | error LE expr{ raise (Parse_error (error_message ())) }
     ;
%%
