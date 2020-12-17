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
open Parsing

# 46 "parser.ml"
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
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\006\000\006\000\007\000\007\000\009\000\009\000\
\010\000\010\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\013\000\013\000\014\000\
\014\000\008\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\005\000\
\005\000\005\000\005\000\005\000\003\000\001\000\001\000\004\000\
\007\000\007\000\005\000\007\000\003\000\000\000\001\000\003\000\
\001\000\004\000\001\000\001\000\004\000\004\000\003\000\003\000\
\003\000\003\000\002\000\003\000\004\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\031\000\072\000\001\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\043\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\000\000\000\000\058\000\051\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\018\000\000\000\005\000\000\000\
\000\000\000\000\000\000\000\000\032\000\019\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\056\000\057\000\000\000\000\000\000\000\059\000\
\052\000\000\000\000\000\049\000\050\000\000\000\000\000\000\000\
\000\000\000\000\000\000\042\000\017\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\024\000\025\000\026\000\053\000\046\000\045\000\027\000\000\000\
\000\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\003\000\004\000\000\000\000\000\000\000\000\000\
\000\000\011\000\033\000\034\000\020\000\036\000\022\000\008\000\
\016\000\000\000\000\000\000\000\010\000\000\000\009\000\015\000"

let yydgoto = "\002\000\
\013\000\014\000\158\000\031\000\071\000\115\000\159\000\015\000\
\160\000\072\000\035\000\042\000\036\000\037\000"

let yysindex = "\004\000\
\150\255\000\000\004\255\011\255\013\255\018\255\023\255\035\255\
\003\255\039\255\000\000\000\000\000\000\000\000\000\000\008\255\
\012\255\041\255\099\255\143\255\071\255\012\255\072\255\115\255\
\000\000\009\255\149\255\161\255\248\255\080\255\088\255\050\255\
\218\255\033\255\151\000\062\255\067\255\007\000\099\000\094\000\
\237\255\073\255\113\000\084\255\085\255\173\255\087\255\012\255\
\012\255\012\255\012\255\000\000\171\255\012\255\000\000\000\000\
\137\000\147\000\012\255\012\255\012\255\012\255\000\000\100\255\
\004\255\113\255\136\255\148\255\000\000\157\255\000\000\124\255\
\137\255\146\255\135\255\012\255\000\000\000\000\012\255\012\255\
\012\255\012\255\012\255\012\255\150\255\012\255\012\255\012\255\
\012\255\012\255\012\255\150\255\150\255\147\255\154\255\156\255\
\082\255\082\255\000\000\000\000\053\000\075\000\159\255\000\000\
\000\000\082\255\082\255\000\000\000\000\160\255\177\255\167\255\
\152\255\162\255\077\255\000\000\000\000\012\255\182\255\000\000\
\151\000\151\000\151\000\151\000\151\000\151\000\151\000\181\255\
\151\000\151\000\151\000\151\000\151\000\151\000\183\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\174\255\
\106\255\106\255\106\255\000\000\207\255\117\000\121\000\133\000\
\150\255\150\255\000\000\000\000\186\255\208\255\189\255\187\255\
\192\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\199\255\106\255\199\255\000\000\228\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\204\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\193\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\247\254\000\000\205\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\204\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\233\255\007\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\215\255\028\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\050\000\072\000\000\000\000\000\000\000\000\000\000\000\
\000\000\092\255\000\000\000\000\000\000\000\000\000\000\000\000\
\249\254\214\255\226\255\243\255\244\255\245\255\249\255\001\000\
\251\255\018\000\021\000\024\000\025\000\038\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\039\000\039\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\227\255\225\255\000\000\000\000\000\000\106\000\151\255\
\000\000\000\000\254\255\235\000\207\000\000\000"

let yytablesize = 431
let yytable = "\070\000\
\035\000\069\000\024\000\025\000\001\000\026\000\029\000\032\000\
\025\000\002\000\026\000\034\000\025\000\033\000\026\000\039\000\
\041\000\041\000\041\000\046\000\040\000\041\000\027\000\040\000\
\056\000\058\000\021\000\027\000\016\000\028\000\017\000\027\000\
\018\000\053\000\028\000\054\000\002\000\019\000\028\000\020\000\
\038\000\025\000\117\000\026\000\021\000\097\000\098\000\099\000\
\100\000\022\000\102\000\048\000\049\000\050\000\051\000\128\000\
\106\000\107\000\108\000\109\000\027\000\023\000\135\000\136\000\
\173\000\030\000\175\000\028\000\048\000\049\000\050\000\051\000\
\045\000\121\000\047\000\073\000\122\000\123\000\124\000\125\000\
\126\000\127\000\064\000\129\000\130\000\131\000\132\000\133\000\
\134\000\075\000\065\000\066\000\004\000\005\000\006\000\007\000\
\008\000\076\000\040\000\025\000\092\000\026\000\061\000\062\000\
\009\000\010\000\148\000\149\000\156\000\066\000\011\000\093\000\
\094\000\157\000\096\000\150\000\152\000\012\000\027\000\067\000\
\068\000\012\000\012\000\166\000\167\000\028\000\003\000\110\000\
\004\000\005\000\006\000\007\000\008\000\048\000\049\000\050\000\
\051\000\111\000\112\000\174\000\009\000\010\000\043\000\025\000\
\052\000\026\000\011\000\116\000\055\000\025\000\113\000\026\000\
\003\000\012\000\004\000\005\000\006\000\007\000\008\000\114\000\
\057\000\025\000\027\000\026\000\120\000\118\000\009\000\010\000\
\027\000\028\000\101\000\025\000\011\000\026\000\119\000\028\000\
\137\000\144\000\146\000\012\000\027\000\151\000\025\000\138\000\
\026\000\139\000\142\000\028\000\147\000\143\000\027\000\059\000\
\060\000\061\000\062\000\145\000\153\000\028\000\154\000\155\000\
\095\000\027\000\044\000\044\000\044\000\044\000\044\000\044\000\
\028\000\162\000\169\000\044\000\044\000\044\000\044\000\168\000\
\170\000\171\000\044\000\172\000\044\000\011\000\044\000\044\000\
\054\000\054\000\054\000\054\000\054\000\054\000\176\000\038\000\
\039\000\054\000\054\000\004\000\059\000\060\000\061\000\062\000\
\054\000\066\000\054\000\074\000\054\000\054\000\086\000\087\000\
\088\000\089\000\090\000\091\000\161\000\067\000\044\000\059\000\
\060\000\061\000\062\000\035\000\103\000\035\000\035\000\035\000\
\035\000\035\000\059\000\060\000\061\000\062\000\068\000\069\000\
\070\000\035\000\035\000\000\000\071\000\063\000\060\000\035\000\
\035\000\048\000\049\000\050\000\051\000\021\000\035\000\021\000\
\021\000\021\000\021\000\021\000\077\000\055\000\055\000\055\000\
\055\000\055\000\055\000\021\000\021\000\061\000\055\000\055\000\
\062\000\021\000\021\000\063\000\064\000\055\000\000\000\055\000\
\021\000\055\000\055\000\047\000\047\000\047\000\047\000\047\000\
\047\000\065\000\013\000\014\000\047\000\047\000\000\000\048\000\
\049\000\050\000\051\000\047\000\000\000\047\000\140\000\047\000\
\047\000\048\000\048\000\048\000\048\000\048\000\048\000\000\000\
\000\000\000\000\048\000\048\000\000\000\059\000\060\000\061\000\
\062\000\048\000\000\000\048\000\141\000\048\000\048\000\079\000\
\080\000\081\000\082\000\083\000\084\000\000\000\000\000\000\000\
\048\000\049\000\050\000\051\000\000\000\059\000\060\000\061\000\
\062\000\085\000\079\000\080\000\081\000\082\000\083\000\084\000\
\078\000\000\000\000\000\048\000\049\000\050\000\051\000\059\000\
\060\000\061\000\062\000\048\000\049\000\050\000\051\000\000\000\
\000\000\000\000\163\000\000\000\000\000\000\000\164\000\059\000\
\060\000\061\000\062\000\048\000\049\000\050\000\051\000\000\000\
\000\000\000\000\165\000\000\000\104\000\059\000\060\000\061\000\
\062\000\059\000\060\000\061\000\062\000\000\000\105\000"

let yycheck = "\031\000\
\000\000\031\000\000\001\001\001\001\000\003\001\009\000\000\001\
\001\001\003\001\003\001\000\001\001\001\016\000\003\001\018\000\
\019\000\020\000\028\001\022\000\028\001\031\001\020\001\031\001\
\027\000\028\000\000\000\020\001\025\001\027\001\027\001\020\001\
\029\001\025\001\027\001\027\001\030\001\027\001\027\001\027\001\
\000\001\001\001\072\000\003\001\027\001\048\000\049\000\050\000\
\051\000\027\001\053\000\019\001\020\001\021\001\022\001\085\000\
\059\000\060\000\061\000\062\000\020\001\027\001\092\000\093\000\
\170\000\027\001\172\000\027\001\019\001\020\001\021\001\022\001\
\002\001\076\000\003\001\026\001\079\000\080\000\081\000\082\000\
\083\000\084\000\003\001\086\000\087\000\088\000\089\000\090\000\
\091\000\028\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\031\001\000\001\001\001\028\001\003\001\021\001\022\001\
\017\001\018\001\030\001\031\001\003\001\004\001\023\001\028\001\
\028\001\145\000\028\001\118\000\119\000\030\001\020\001\032\001\
\033\001\030\001\031\001\153\000\154\000\027\001\003\001\028\001\
\005\001\006\001\007\001\008\001\009\001\019\001\020\001\021\001\
\022\001\025\001\003\001\171\000\017\001\018\001\000\001\001\001\
\030\001\003\001\023\001\024\001\000\001\001\001\003\001\003\001\
\003\001\030\001\005\001\006\001\007\001\008\001\009\001\003\001\
\000\001\001\001\020\001\003\001\030\001\029\001\017\001\018\001\
\020\001\027\001\000\001\001\001\023\001\003\001\029\001\027\001\
\030\001\001\001\027\001\030\001\020\001\000\001\001\001\030\001\
\003\001\030\001\028\001\027\001\027\001\030\001\020\001\019\001\
\020\001\021\001\022\001\029\001\016\001\027\001\016\001\026\001\
\028\001\020\001\010\001\011\001\012\001\013\001\014\001\015\001\
\027\001\003\001\003\001\019\001\020\001\021\001\022\001\030\001\
\028\001\031\001\026\001\028\001\028\001\023\001\030\001\031\001\
\010\001\011\001\012\001\013\001\014\001\015\001\003\001\028\001\
\028\001\019\001\020\001\003\001\019\001\020\001\021\001\022\001\
\026\001\028\001\028\001\026\001\030\001\031\001\010\001\011\001\
\012\001\013\001\014\001\015\001\147\000\028\001\020\000\019\001\
\020\001\021\001\022\001\003\001\054\000\005\001\006\001\007\001\
\008\001\009\001\019\001\020\001\021\001\022\001\028\001\028\001\
\028\001\017\001\018\001\255\255\028\001\030\001\028\001\023\001\
\024\001\019\001\020\001\021\001\022\001\003\001\030\001\005\001\
\006\001\007\001\008\001\009\001\030\001\010\001\011\001\012\001\
\013\001\014\001\015\001\017\001\018\001\028\001\019\001\020\001\
\028\001\023\001\024\001\028\001\028\001\026\001\255\255\028\001\
\030\001\030\001\031\001\010\001\011\001\012\001\013\001\014\001\
\015\001\028\001\028\001\028\001\019\001\020\001\255\255\019\001\
\020\001\021\001\022\001\026\001\255\255\028\001\026\001\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\255\255\
\255\255\255\255\019\001\020\001\255\255\019\001\020\001\021\001\
\022\001\026\001\255\255\028\001\026\001\030\001\031\001\010\001\
\011\001\012\001\013\001\014\001\015\001\255\255\255\255\255\255\
\019\001\020\001\021\001\022\001\255\255\019\001\020\001\021\001\
\022\001\028\001\010\001\011\001\012\001\013\001\014\001\015\001\
\030\001\255\255\255\255\019\001\020\001\021\001\022\001\019\001\
\020\001\021\001\022\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\030\001\255\255\255\255\255\255\030\001\019\001\
\020\001\021\001\022\001\019\001\020\001\021\001\022\001\255\255\
\255\255\255\255\030\001\255\255\028\001\019\001\020\001\021\001\
\022\001\019\001\020\001\021\001\022\001\255\255\028\001"

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
# 347 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "parser.mly"
           ( IntTyp )
# 353 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 33 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 360 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 34 "parser.mly"
               ( NameTyp _1 )
# 367 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 38 "parser.mly"
                ( _1@_2 )
# 375 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                ( [] )
# 381 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 42 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 389 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 43 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 397 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 407 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 416 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "parser.mly"
                       ( _1@[_3] )
# 424 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                       ( [_1]  )
# 431 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
                        ( [] )
# 437 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 55 "parser.mly"
                        ( _1 )
# 444 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 58 "parser.mly"
                             ( _1@[(_3,_4)] )
# 453 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                             ( [(_1,_2)] )
# 461 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                   ( _1@[_2] )
# 469 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                   ( [_1] )
# 476 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
                              ( Assign (Var _1, _3) )
# 484 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 493 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                              ( If (_3, _5, None) )
# 501 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 70 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 510 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 71 "parser.mly"
                              ( While (_3, _5) )
# 518 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 72 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 525 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 73 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 532 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 74 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 539 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 75 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 546 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 76 "parser.mly"
                                ( CallProc (_1, _3) )
# 554 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 561 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 78 "parser.mly"
             ( _1 )
# 568 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
            ( NilStmt )
# 574 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 80 "parser.mly"
                               ( print_error_message (); ErrorStmt )
# 581 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                                       ( print_error_message (); ErrorStmt )
# 589 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                                       ( print_error_message (); ErrorStmt )
# 597 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 83 "parser.mly"
                               ( print_error_message (); ErrorStmt )
# 604 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 84 "parser.mly"
                                     ( print_error_message (); ErrorStmt )
# 612 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                            ( print_error_message (); ErrorStmt )
# 618 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                           ( [] )
# 624 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 89 "parser.mly"
                           ( _1 )
# 631 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                          ( _1@[_3] )
# 639 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                           ( [_1] )
# 646 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 96 "parser.mly"
                         ( Block (_2, _3) )
# 654 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 99 "parser.mly"
           ( IntExp _1  )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
          ( VarExp (Var _1) )
# 668 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 101 "parser.mly"
                          ( CallFunc (_1, _3) )
# 676 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 684 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 692 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 700 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 708 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 716 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 723 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                   ( _2 )
# 730 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    Obj.repr(
# 109 "parser.mly"
                      ( print_error_message (); ErrorStmt )
# 737 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                       ( print_error_message (); ErrorStmt )
# 744 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                        ( print_error_message (); ErrorStmt )
# 751 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                        ( print_error_message (); ErrorStmt )
# 758 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                      ( print_error_message (); ErrorStmt )
# 765 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                                ( print_error_message (); ErrorStmt )
# 771 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                    ( print_error_message (); ErrorStmt )
# 777 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 785 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 793 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 801 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 809 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 817 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 825 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "parser.mly"
                     ( print_error_message (); ErrorStmt )
# 832 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "parser.mly"
                      ( print_error_message (); ErrorStmt )
# 839 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "parser.mly"
                     ( print_error_message (); ErrorStmt )
# 846 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "parser.mly"
                     ( print_error_message (); ErrorStmt )
# 853 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 128 "parser.mly"
                     ( print_error_message (); ErrorStmt )
# 860 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "parser.mly"
                    ( print_error_message (); ErrorStmt )
# 867 "parser.ml"
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
