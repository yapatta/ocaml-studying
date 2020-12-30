%{

open Printf
open Ast
open Lexer

%}

/* File parser.mly */
%token <int> NUM
%token <string> STR ID
%token INT IF WHILE SPRINT IPRINT SCAN EQ NEQ GT LT GE LE ELSE RETURN NEW
%token PLUS INC MINUS TIMES DIV MOD POW LB RB LS RS LP RP ASSIGN INC_ASSIGN SEMI COMMA TYPE VOID
%type <Ast.stmt> prog


%nonassoc GT LT EQ NEQ GE LE
%left PLUS MINUS         /* lowest precedence */
%left TIMES DIV MOD POW        /* medium precedence */
%nonassoc UMINUS INC     /* highest precedence */


%start prog           /* the entry point */

%%

prog : stmt  {  $1  }
     ;

ty   : INT { IntTyp }
     | INT LS NUM RS { ArrayTyp ($3, IntTyp) }
     | ID	     { NameTyp $1 }
     ;

decs : decs dec { let (decs, stmts) = $1 in let (d, s) = $2 in (decs@d, stmts@s) }
     |          { ([], []) }
     ;

dec  : ty ids SEMI   { let (d, stmts) = $2 in let decs = List.map (fun x -> VarDec ($1,x)) d in (decs, stmts) }
     | TYPE ID ASSIGN ty SEMI { ([TypeDec ($2,$4)], []) }
     | ty ID LP fargs_opt RP block  { ([FuncDec($2, $4, $1, $6)], []) }
     | VOID ID LP fargs_opt RP block  { ([FuncDec($2, $4, VoidTyp, $6)], []) }
     ; 

ids  : ids COMMA ID    { let (decs, stmts) = $1 in (decs@[$3], stmts) }
     | ids COMMA ID ASSIGN expr { let (decs, stmts) = $1 in (decs@[$3], stmts@[Assign(Var $3, $5)]) }
     | ID              { ([$1],[]) }
     | ID ASSIGN expr       { ([$1],[Assign(Var $1, $3)]) }
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
     | ID INC_ASSIGN expr SEMI { IncAssign (Var $1, $3) }
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
     ;

aargs_opt: /* empty */     { [] }
        | aargs            { $1 }
        ;

aargs : aargs COMMA expr  { $1@[$3] }
      | expr               { [$1] }
      ;

block: LB decs stmts RB  { let (decs, stmts) = $2 in Block (decs, stmts@$3) }
     ;

expr : NUM { IntExp $1  }
     | ID { VarExp (Var $1) }
     | ID LP aargs_opt RP { CallFunc ($1, $3) } 
     | ID LS expr RS  { VarExp (IndexedVar (Var $1, $3)) }
     | expr PLUS expr { CallFunc ("+", [$1; $3]) }
     | ID INC { CallFunc ("++", [VarExp (Var $1)]) }
     | expr MINUS expr { CallFunc ("-", [$1; $3]) }
     | expr TIMES expr { CallFunc ("*", [$1; $3]) }
     | expr DIV expr { CallFunc ("/", [$1; $3]) }
     | expr MOD expr { CallFunc ("-", [$1; CallFunc ("*", [CallFunc ("/", [$1; $3]); $3])]) }
     | expr POW expr { CallFunc ("^", [$1; $3])}
     | MINUS expr %prec UMINUS { CallFunc("!", [$2]) }
     | LP expr RP  { $2 }
     ;

cond : expr EQ expr  { CallFunc ("==", [$1; $3]) }
     | expr NEQ expr { CallFunc ("!=", [$1; $3]) }
     | expr GT expr  { CallFunc (">", [$1; $3]) }
     | expr LT expr  { CallFunc ("<", [$1; $3]) }
     | expr GE expr  { CallFunc (">=", [$1; $3]) }
     | expr LE expr  { CallFunc ("<=", [$1; $3]) }
     ;
%%
