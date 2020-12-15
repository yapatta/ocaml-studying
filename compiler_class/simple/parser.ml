type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | NEW
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LB
  | RB
  | LS
  | RS
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA
  | TYPE
  | VOID

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Printf
open Ast
open Lexer
exception Parse_error of string
let error_message () = Printf.sprintf "At line %d, Before lexical unit: %s" !Lexer.line_num !Lexer.before_lex

# 47 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* NEW *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* LB *);
  280 (* RB *);
  281 (* LS *);
  282 (* RS *);
  283 (* LP *);
  284 (* RP *);
  285 (* ASSIGN *);
  286 (* SEMI *);
  287 (* COMMA *);
  288 (* TYPE *);
  289 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\003\000\004\000\004\000\005\000\
\005\000\005\000\005\000\005\000\005\000\006\000\006\000\007\000\
\007\000\009\000\009\000\010\000\010\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\013\000\013\000\
\014\000\014\000\014\000\008\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\004\000\002\000\000\000\003\000\
\005\000\006\000\006\000\006\000\006\000\003\000\001\000\000\000\
\001\000\004\000\002\000\002\000\001\000\004\000\007\000\005\000\
\007\000\005\000\005\000\005\000\005\000\005\000\005\000\003\000\
\001\000\001\000\004\000\007\000\005\000\007\000\000\000\001\000\
\003\000\001\000\003\000\004\000\001\000\001\000\004\000\004\000\
\003\000\003\000\003\000\003\000\002\000\003\000\004\000\004\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\007\000\034\000\075\000\001\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\061\000\053\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\000\060\000\000\000\000\000\000\000\000\000\
\062\000\054\000\000\000\000\000\051\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\020\000\000\000\000\000\
\000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\027\000\028\000\029\000\056\000\048\000\055\000\
\047\000\030\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\000\000\000\000\005\000\003\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\036\000\023\000\038\000\025\000\009\000\000\000\019\000\000\000\
\000\000\000\000\000\000\013\000\011\000\000\000\012\000\010\000\
\018\000"

let yydgoto = "\002\000\
\013\000\014\000\163\000\031\000\070\000\116\000\164\000\015\000\
\165\000\071\000\035\000\042\000\036\000\037\000"

let yysindex = "\014\000\
\159\255\000\000\022\255\252\254\023\255\046\255\050\255\053\255\
\004\255\058\255\000\000\000\000\000\000\000\000\000\000\013\255\
\045\255\054\255\075\255\152\255\119\255\004\255\120\255\164\000\
\000\000\009\255\158\255\170\255\226\255\123\255\102\255\053\000\
\075\000\093\255\168\000\094\255\106\255\248\255\007\000\094\000\
\113\000\103\255\126\000\105\255\115\255\182\255\117\255\004\255\
\004\255\004\255\004\255\180\255\191\255\000\000\000\000\134\000\
\144\000\004\255\004\255\004\255\004\255\000\000\126\255\022\255\
\124\255\157\255\166\255\000\000\171\255\000\000\133\255\146\255\
\155\255\004\255\156\255\004\255\000\000\000\000\004\255\004\255\
\004\255\004\255\004\255\004\255\159\255\004\255\004\255\004\255\
\004\255\004\255\004\255\159\255\159\255\163\255\165\255\168\255\
\031\255\031\255\000\000\000\000\148\000\156\000\246\254\160\255\
\000\000\000\000\031\255\031\255\000\000\000\000\169\255\066\255\
\167\255\178\255\179\255\057\255\000\000\000\000\004\255\004\255\
\168\000\000\000\168\000\168\000\168\000\168\000\168\000\168\000\
\168\000\192\255\168\000\168\000\168\000\168\000\168\000\168\000\
\193\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\161\255\194\255\100\255\079\255\144\255\000\000\
\216\255\099\000\130\000\159\255\159\255\000\000\000\000\000\000\
\195\255\198\255\228\255\199\255\209\255\201\255\213\255\000\000\
\000\000\000\000\000\000\000\000\000\000\219\255\000\000\219\255\
\100\255\219\255\219\255\000\000\000\000\246\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\223\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\202\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\231\254\000\000\229\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\223\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\250\255\
\005\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\224\255\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\050\000\072\000\000\000\000\000\000\000\000\000\
\000\000\000\000\086\255\000\000\000\000\000\000\000\000\000\000\
\245\254\000\000\250\254\230\255\231\255\233\255\243\255\244\255\
\245\255\001\000\249\255\251\255\018\000\021\000\024\000\025\000\
\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\038\000\038\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\039\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\227\255\225\255\000\000\000\000\000\000\125\000\121\255\
\000\000\000\000\010\000\035\001\015\001\000\000"

let yytablesize = 446
let yytable = "\069\000\
\037\000\068\000\042\000\024\000\025\000\042\000\026\000\002\000\
\048\000\049\000\050\000\051\000\032\000\025\000\001\000\026\000\
\043\000\144\000\029\000\043\000\074\000\041\000\019\000\027\000\
\041\000\033\000\024\000\039\000\041\000\041\000\028\000\046\000\
\027\000\052\000\002\000\053\000\055\000\057\000\180\000\028\000\
\181\000\118\000\183\000\184\000\034\000\025\000\016\000\026\000\
\017\000\020\000\018\000\060\000\061\000\038\000\025\000\130\000\
\026\000\097\000\098\000\099\000\100\000\102\000\137\000\138\000\
\027\000\147\000\148\000\107\000\108\000\109\000\110\000\028\000\
\021\000\027\000\040\000\025\000\022\000\026\000\162\000\023\000\
\028\000\160\000\065\000\121\000\030\000\123\000\152\000\153\000\
\124\000\125\000\126\000\127\000\128\000\129\000\027\000\131\000\
\132\000\133\000\134\000\135\000\136\000\028\000\160\000\065\000\
\064\000\065\000\004\000\005\000\006\000\007\000\008\000\048\000\
\049\000\050\000\051\000\015\000\015\000\161\000\009\000\010\000\
\045\000\075\000\047\000\074\000\011\000\063\000\171\000\172\000\
\154\000\155\000\092\000\012\000\093\000\066\000\067\000\003\000\
\076\000\004\000\005\000\006\000\007\000\008\000\094\000\166\000\
\096\000\182\000\160\000\065\000\112\000\009\000\010\000\043\000\
\025\000\111\000\026\000\011\000\117\000\054\000\025\000\113\000\
\026\000\003\000\012\000\004\000\005\000\006\000\007\000\008\000\
\114\000\056\000\025\000\027\000\026\000\115\000\119\000\009\000\
\010\000\027\000\028\000\101\000\025\000\011\000\026\000\120\000\
\028\000\122\000\158\000\145\000\012\000\027\000\103\000\025\000\
\139\000\026\000\140\000\149\000\028\000\141\000\146\000\027\000\
\058\000\059\000\060\000\061\000\150\000\151\000\028\000\156\000\
\157\000\095\000\027\000\046\000\046\000\046\000\046\000\046\000\
\046\000\028\000\168\000\159\000\046\000\046\000\046\000\046\000\
\173\000\174\000\176\000\046\000\178\000\046\000\175\000\046\000\
\046\000\057\000\057\000\057\000\057\000\057\000\057\000\177\000\
\179\000\011\000\057\000\057\000\058\000\059\000\060\000\061\000\
\185\000\057\000\039\000\057\000\004\000\057\000\057\000\062\000\
\040\000\069\000\070\000\037\000\071\000\037\000\037\000\037\000\
\037\000\037\000\048\000\049\000\050\000\051\000\072\000\073\000\
\074\000\037\000\037\000\167\000\063\000\077\000\064\000\037\000\
\037\000\058\000\059\000\060\000\061\000\024\000\037\000\024\000\
\024\000\024\000\024\000\024\000\078\000\058\000\058\000\058\000\
\058\000\058\000\058\000\024\000\024\000\065\000\058\000\058\000\
\066\000\024\000\024\000\067\000\068\000\058\000\044\000\058\000\
\024\000\058\000\058\000\049\000\049\000\049\000\049\000\049\000\
\049\000\016\000\017\000\104\000\049\000\049\000\000\000\048\000\
\049\000\050\000\051\000\049\000\000\000\049\000\072\000\049\000\
\049\000\050\000\050\000\050\000\050\000\050\000\050\000\000\000\
\000\000\000\000\050\000\050\000\000\000\058\000\059\000\060\000\
\061\000\050\000\000\000\050\000\073\000\050\000\050\000\079\000\
\080\000\081\000\082\000\083\000\084\000\000\000\000\000\000\000\
\048\000\049\000\050\000\051\000\000\000\058\000\059\000\060\000\
\061\000\085\000\086\000\087\000\088\000\089\000\090\000\091\000\
\169\000\000\000\000\000\058\000\059\000\060\000\061\000\079\000\
\080\000\081\000\082\000\083\000\084\000\000\000\000\000\000\000\
\048\000\049\000\050\000\051\000\058\000\059\000\060\000\061\000\
\048\000\049\000\050\000\051\000\000\000\000\000\000\000\170\000\
\000\000\105\000\058\000\059\000\060\000\061\000\048\000\049\000\
\050\000\051\000\000\000\106\000\000\000\142\000\058\000\059\000\
\060\000\061\000\000\000\000\000\000\000\143\000\048\000\049\000\
\050\000\051\000\058\000\059\000\060\000\061\000"

let yycheck = "\031\000\
\000\000\031\000\028\001\000\001\001\001\031\001\003\001\003\001\
\019\001\020\001\021\001\022\001\000\001\001\001\001\000\003\001\
\028\001\028\001\009\000\031\001\031\001\028\001\027\001\020\001\
\031\001\016\000\000\000\018\000\019\000\020\000\027\001\022\000\
\020\001\025\001\030\001\027\001\027\000\028\000\174\000\027\001\
\176\000\071\000\178\000\179\000\000\001\001\001\025\001\003\001\
\027\001\027\001\029\001\021\001\022\001\000\001\001\001\085\000\
\003\001\048\000\049\000\050\000\051\000\052\000\092\000\093\000\
\020\001\000\001\001\001\058\000\059\000\060\000\061\000\027\001\
\027\001\020\001\000\001\001\001\027\001\003\001\000\001\027\001\
\027\001\003\001\004\001\074\000\027\001\076\000\030\001\031\001\
\079\000\080\000\081\000\082\000\083\000\084\000\020\001\086\000\
\087\000\088\000\089\000\090\000\091\000\027\001\003\001\004\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\019\001\
\020\001\021\001\022\001\030\001\031\001\149\000\017\001\018\001\
\002\001\028\001\003\001\031\001\023\001\003\001\156\000\157\000\
\119\000\120\000\028\001\030\001\028\001\032\001\033\001\003\001\
\031\001\005\001\006\001\007\001\008\001\009\001\028\001\000\001\
\028\001\177\000\003\001\004\001\025\001\017\001\018\001\000\001\
\001\001\028\001\003\001\023\001\024\001\000\001\001\001\003\001\
\003\001\003\001\030\001\005\001\006\001\007\001\008\001\009\001\
\003\001\000\001\001\001\020\001\003\001\003\001\029\001\017\001\
\018\001\020\001\027\001\000\001\001\001\023\001\003\001\029\001\
\027\001\030\001\026\001\028\001\030\001\020\001\000\001\001\001\
\030\001\003\001\030\001\029\001\027\001\030\001\030\001\020\001\
\019\001\020\001\021\001\022\001\027\001\027\001\027\001\016\001\
\016\001\028\001\020\001\010\001\011\001\012\001\013\001\014\001\
\015\001\027\001\003\001\026\001\019\001\020\001\021\001\022\001\
\030\001\028\001\028\001\026\001\028\001\028\001\003\001\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\031\001\
\028\001\023\001\019\001\020\001\019\001\020\001\021\001\022\001\
\003\001\026\001\028\001\028\001\003\001\030\001\031\001\030\001\
\028\001\028\001\028\001\003\001\028\001\005\001\006\001\007\001\
\008\001\009\001\019\001\020\001\021\001\022\001\028\001\028\001\
\028\001\017\001\018\001\151\000\028\001\030\001\028\001\023\001\
\024\001\019\001\020\001\021\001\022\001\003\001\030\001\005\001\
\006\001\007\001\008\001\009\001\030\001\010\001\011\001\012\001\
\013\001\014\001\015\001\017\001\018\001\028\001\019\001\020\001\
\028\001\023\001\024\001\028\001\028\001\026\001\020\000\028\001\
\030\001\030\001\031\001\010\001\011\001\012\001\013\001\014\001\
\015\001\028\001\028\001\053\000\019\001\020\001\255\255\019\001\
\020\001\021\001\022\001\026\001\255\255\028\001\026\001\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\255\255\019\001\020\001\021\001\
\022\001\026\001\255\255\028\001\026\001\030\001\031\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\019\001\020\001\021\001\
\022\001\028\001\010\001\011\001\012\001\013\001\014\001\015\001\
\030\001\255\255\255\255\019\001\020\001\021\001\022\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\019\001\020\001\021\001\022\001\
\019\001\020\001\021\001\022\001\255\255\255\255\255\255\030\001\
\255\255\028\001\019\001\020\001\021\001\022\001\019\001\020\001\
\021\001\022\001\255\255\028\001\255\255\026\001\019\001\020\001\
\021\001\022\001\255\255\255\255\255\255\026\001\019\001\020\001\
\021\001\022\001\019\001\020\001\021\001\022\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  NEW\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LB\000\
  RB\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  TYPE\000\
  VOID\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 29 "parser.mly"
             (  _1  )
# 360 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
           ( IntTyp )
# 366 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 33 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 373 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
               ( NameTyp _1 )
# 380 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                       ( raise (Parse_error (error_message ())) )
# 386 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 38 "parser.mly"
                ( _1@_2 )
# 394 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                ( [] )
# 400 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 42 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 408 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 43 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 416 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 426 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 435 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                ( raise (Parse_error (error_message ())) )
# 444 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 47 "parser.mly"
                                  ( raise (Parse_error (error_message ())) )
# 452 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( _1@[_3] )
# 460 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                       ( [_1]  )
# 467 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                        ( [] )
# 473 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 55 "parser.mly"
                        ( _1 )
# 480 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( _1@[(_3,_4)] )
# 489 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                             ( [(_1,_2)] )
# 497 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( _1@[_2] )
# 505 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                   ( [_1] )
# 512 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                              ( Assign (Var _1, _3) )
# 520 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 529 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                              ( If (_3, _5, None) )
# 537 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 546 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                              ( While (_3, _5) )
# 554 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 72 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 561 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 575 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 582 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 76 "parser.mly"
                                ( CallProc (_1, _3) )
# 590 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 597 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 78 "parser.mly"
             ( _1 )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ( NilStmt )
# 610 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 80 "parser.mly"
                               ( raise (Parse_error (error_message ())) )
# 617 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                                       ( raise (Parse_error (error_message ())) )
# 625 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 82 "parser.mly"
                               ( raise (Parse_error (error_message ())) )
# 632 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                              ( raise (Parse_error (error_message ())) )
# 640 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                           ( [] )
# 646 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 88 "parser.mly"
                           ( _1 )
# 653 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                          ( _1@[_3] )
# 661 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                           ( [_1] )
# 668 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                          ( raise (Parse_error (error_message ())) )
# 675 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 96 "parser.mly"
                         ( Block (_2, _3) )
# 683 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "parser.mly"
           ( IntExp _1  )
# 690 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
          ( VarExp (Var _1) )
# 697 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 101 "parser.mly"
                          ( CallFunc (_1, _3) )
# 705 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 713 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 721 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 729 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 745 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 752 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                   ( _2 )
# 759 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 109 "parser.mly"
                      ( raise (Parse_error (error_message ())) )
# 766 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 110 "parser.mly"
                      ( raise (Parse_error (error_message ())) )
# 773 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                       ( raise (Parse_error (error_message ())) )
# 780 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                        ( raise (Parse_error (error_message ())) )
# 787 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                        ( raise (Parse_error (error_message ())) )
# 794 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                      ( raise (Parse_error (error_message ())) )
# 801 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                                ( raise (Parse_error (error_message ())) )
# 807 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 116 "parser.mly"
                   ( raise (Parse_error (error_message ())) )
# 813 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 821 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 829 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 837 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 845 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 853 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 861 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                     ( raise (Parse_error (error_message ())) )
# 868 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                      ( raise (Parse_error (error_message ())) )
# 875 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( raise (Parse_error (error_message ())) )
# 882 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( raise (Parse_error (error_message ())) )
# 889 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                     ( raise (Parse_error (error_message ())) )
# 896 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "parser.mly"
                    ( raise (Parse_error (error_message ())) )
# 903 "parser.ml"
               : 'cond))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
;;
