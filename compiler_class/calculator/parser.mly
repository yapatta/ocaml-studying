%token<int> NUM
%token PLUS MINUS TIMES DIV LP RP EOL

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start prog
%type <int> prog
%%
prog: expr EOL                   { $1 }
    ;

expr: NUM                        { $1 }
    | LP expr RP                 { $2 }
    | expr PLUS expr             { $1 + $3 }
    | expr MINUS expr            { $1 - $3 } 
    | expr TIMES expr            { $1 * $3 }
    | expr DIV expr              { $1 / $3 }
    | MINUS expr %prec UMINUS    {-$2 }
    ;
