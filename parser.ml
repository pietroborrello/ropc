type token =
  | NEWLINE
  | EOF
  | EQ
  | COMMA
  | BANG
  | LPAREN
  | RPAREN
  | LCURLY
  | RCURLY
  | LBRACKET
  | RBRACKET
  | NUM of (int)
  | LABEL of (string)
  | BRANCH of (string)
  | DOLLAR
  | AT
  | PLUS
  | MINUS
  | MUL
  | DIV
  | XOR
  | OR
  | AND
  | NOT
  | STR of (string)
  | ID of (string)
  | FNCT of (float->float)
  | FUN
  | CMP

open Parsing;;
# 2 "parser.mly"

open Printf
open Lexing

open Ast

(* get meta / save meta *)
let get_meta n = 
    let p = Parsing.rhs_start_pos n in
    let lnum = p.pos_lnum in
    (* let col = p.pos_cnum - p.pos_bol in *)
    {lnum=lnum} 

let wrap node = 
    let meta = get_meta 1 in
    {n=node; m=meta}

let only_small l = 
    let p x = x>255 in
    try let _ = List.find p l in false with Not_found -> true

let unescape s =
  Scanf.sscanf ("\"" ^ s ^ "\"") "%S" (fun u -> u)

let explode s =
  let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) ['\x00']

let str_to_byte_list s = 
    let s = unescape s in
    let l = explode s in
    let l = List.map (fun c -> Char.code c) l in
    l

let assign_tab id l =
    if only_small l then
        let s = AssignTab(id, l) in 
        s
    else failwith "Only byte values (0-255) allowed in tabs"

# 76 "parser.ml"
let yytransl_const = [|
  257 (* NEWLINE *);
    0 (* EOF *);
  258 (* EQ *);
  259 (* COMMA *);
  260 (* BANG *);
  261 (* LPAREN *);
  262 (* RPAREN *);
  263 (* LCURLY *);
  264 (* RCURLY *);
  265 (* LBRACKET *);
  266 (* RBRACKET *);
  270 (* DOLLAR *);
  271 (* AT *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* MUL *);
  275 (* DIV *);
  276 (* XOR *);
  277 (* OR *);
  278 (* AND *);
  279 (* NOT *);
  283 (* FUN *);
  284 (* CMP *);
    0|]

let yytransl_block = [|
  267 (* NUM *);
  268 (* LABEL *);
  269 (* BRANCH *);
  280 (* STR *);
  281 (* ID *);
  282 (* FNCT *);
    0|]

let yylhs = "\255\255\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\004\000\006\000\006\000\007\000\007\000\007\000\008\000\
\008\000\008\000\009\000\005\000\010\000\011\000\012\000\012\000\
\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\002\000\003\000\003\000\001\000\
\003\000\005\000\003\000\004\000\001\000\002\000\004\000\005\000\
\002\000\003\000\002\000\001\000\003\000\001\000\000\000\003\000\
\001\000\000\000\003\000\003\000\003\000\004\000\002\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\041\000\000\000\043\000\040\000\000\000\000\000\
\039\000\000\000\000\000\030\000\000\000\000\000\038\000\000\000\
\035\000\000\000\000\000\021\000\000\000\000\000\000\000\000\000\
\028\000\000\000\029\000\000\000\000\000\022\000\000\000\000\000\
\000\000\025\000\000\000\000\000\001\000\000\000\000\000\000\000\
\002\000\000\000\037\000\027\000\026\000\000\000\000\000\000\000\
\017\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\012\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\016\000\000\000\000\000\036\000\014\000\
\011\000\000\000\000\000\000\000\000\000\000\000\008\000\009\000\
\010\000\000\000\000\000\018\000\000\000\015\000"

let yydgoto = "\002\000\
\005\000\042\000\069\000\025\000\034\000\026\000\013\000\052\000\
\011\000\015\000\006\000\007\000"

let yysindex = "\006\000\
\001\000\000\000\000\000\239\254\000\000\000\000\247\254\021\255\
\000\000\002\255\030\255\000\000\009\255\182\255\000\000\025\255\
\000\000\043\255\057\255\000\000\061\255\075\255\011\255\200\255\
\000\000\125\255\000\000\041\255\054\255\000\000\110\255\197\255\
\200\255\000\000\200\255\079\255\000\000\080\255\200\255\200\255\
\000\000\003\255\000\000\000\000\000\000\116\255\200\255\245\254\
\000\000\217\255\217\255\039\255\210\255\117\255\000\000\102\255\
\000\000\200\255\200\255\200\255\200\255\200\255\200\255\200\255\
\200\255\200\255\217\255\000\000\001\255\200\255\000\000\000\000\
\000\000\217\255\102\255\102\255\224\255\224\255\000\000\000\000\
\000\000\217\255\119\255\000\000\217\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\
\000\000\052\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\070\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\132\255\088\255\000\000\000\000\000\000\000\000\035\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\150\255\000\000\000\000\000\000\000\000\000\000\
\000\000\157\255\053\255\071\255\089\255\107\255\000\000\000\000\
\000\000\175\255\000\000\000\000\103\255\000\000"

let yygindex = "\000\000\
\000\000\226\255\000\000\105\000\114\000\000\000\000\000\000\000\
\000\000\000\000\136\000\000\000"

let yytablesize = 284
let yytable = "\068\000\
\003\000\050\000\051\000\083\000\053\000\058\000\001\000\008\000\
\056\000\057\000\084\000\016\000\032\000\054\000\017\000\033\000\
\067\000\004\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\010\000\012\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\014\000\013\000\013\000\085\000\
\013\000\070\000\013\000\013\000\071\000\033\000\013\000\013\000\
\013\000\027\000\013\000\013\000\013\000\013\000\031\000\004\000\
\004\000\031\000\004\000\013\000\004\000\004\000\013\000\046\000\
\004\000\004\000\004\000\028\000\004\000\004\000\004\000\004\000\
\034\000\005\000\005\000\034\000\005\000\004\000\005\000\005\000\
\004\000\029\000\005\000\005\000\005\000\030\000\005\000\005\000\
\005\000\005\000\033\000\006\000\006\000\033\000\006\000\005\000\
\006\000\006\000\005\000\031\000\006\000\006\000\006\000\054\000\
\055\000\032\000\006\000\006\000\032\000\007\000\007\000\047\000\
\007\000\006\000\007\000\007\000\006\000\066\000\007\000\007\000\
\007\000\063\000\064\000\065\000\007\000\007\000\073\000\042\000\
\018\000\086\000\044\000\007\000\043\000\019\000\007\000\019\000\
\020\000\021\000\022\000\019\000\019\000\045\000\009\000\019\000\
\019\000\019\000\000\000\000\000\000\000\023\000\000\000\000\000\
\024\000\020\000\000\000\000\000\019\000\020\000\020\000\019\000\
\023\000\020\000\020\000\020\000\023\000\023\000\000\000\000\000\
\023\000\023\000\023\000\000\000\000\000\000\000\020\000\000\000\
\000\000\020\000\024\000\000\000\000\000\023\000\024\000\024\000\
\023\000\018\000\024\000\024\000\024\000\000\000\019\000\000\000\
\000\000\020\000\021\000\022\000\000\000\000\000\000\000\024\000\
\000\000\035\000\024\000\000\000\035\000\048\000\023\000\037\000\
\036\000\024\000\037\000\038\000\000\000\039\000\038\000\072\000\
\039\000\000\000\000\000\040\000\049\000\041\000\040\000\000\000\
\041\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\059\000\
\060\000\000\000\000\000\063\000\064\000\065\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000"

let yycheck = "\011\001\
\000\000\032\000\033\000\003\001\035\000\003\001\001\000\025\001\
\039\000\040\000\010\001\003\001\002\001\025\001\006\001\005\001\
\047\000\027\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\005\001\025\001\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\007\001\003\001\004\001\070\000\
\006\001\003\001\008\001\009\001\006\001\005\001\012\001\013\001\
\014\001\025\001\016\001\017\001\018\001\019\001\003\001\003\001\
\004\001\006\001\006\001\025\001\008\001\009\001\028\001\010\001\
\012\001\013\001\014\001\025\001\016\001\017\001\018\001\019\001\
\003\001\003\001\004\001\006\001\006\001\025\001\008\001\009\001\
\028\001\025\001\012\001\013\001\014\001\025\001\016\001\017\001\
\018\001\019\001\003\001\003\001\004\001\006\001\006\001\025\001\
\008\001\009\001\028\001\025\001\012\001\013\001\014\001\025\001\
\025\001\003\001\018\001\019\001\006\001\003\001\004\001\002\001\
\006\001\025\001\008\001\009\001\028\001\002\001\012\001\013\001\
\014\001\020\001\021\001\022\001\018\001\019\001\010\001\000\000\
\004\001\011\001\026\000\025\001\008\001\009\001\028\001\004\001\
\012\001\013\001\014\001\008\001\009\001\028\000\007\000\012\001\
\013\001\014\001\255\255\255\255\255\255\025\001\255\255\255\255\
\028\001\004\001\255\255\255\255\025\001\008\001\009\001\028\001\
\004\001\012\001\013\001\014\001\008\001\009\001\255\255\255\255\
\012\001\013\001\014\001\255\255\255\255\255\255\025\001\255\255\
\255\255\028\001\004\001\255\255\255\255\025\001\008\001\009\001\
\028\001\004\001\012\001\013\001\014\001\255\255\009\001\255\255\
\255\255\012\001\013\001\014\001\255\255\255\255\255\255\025\001\
\255\255\005\001\028\001\255\255\005\001\009\001\025\001\011\001\
\009\001\028\001\011\001\015\001\255\255\017\001\015\001\006\001\
\017\001\255\255\255\255\023\001\024\001\025\001\023\001\255\255\
\025\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\016\001\
\017\001\255\255\255\255\020\001\021\001\022\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\027\001"

let yynames_const = "\
  NEWLINE\000\
  EOF\000\
  EQ\000\
  COMMA\000\
  BANG\000\
  LPAREN\000\
  RPAREN\000\
  LCURLY\000\
  RCURLY\000\
  LBRACKET\000\
  RBRACKET\000\
  DOLLAR\000\
  AT\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  XOR\000\
  OR\000\
  AND\000\
  NOT\000\
  FUN\000\
  CMP\000\
  "

let yynames_block = "\
  NUM\000\
  LABEL\000\
  BRANCH\000\
  STR\000\
  ID\000\
  FNCT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 74 "parser.mly"
                        ( let e = Const(_1) in e )
# 294 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "parser.mly"
                        ( let e = Var(_1) in e )
# 301 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
                        ( let e = Ref(_2) in e )
# 308 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 77 "parser.mly"
                        ( let e = BinOp(_1, Add, _3) in e )
# 316 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 78 "parser.mly"
                        ( let e = BinOp(_1, Sub, _3) in e )
# 324 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 79 "parser.mly"
                        ( let e = BinOp(_1, Mul, _3) in e )
# 332 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 80 "parser.mly"
                        ( let e = BinOp(_1, Div, _3) in e )
# 340 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 81 "parser.mly"
                        ( let e = BinOp(_1, Xor, _3) in e )
# 348 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 82 "parser.mly"
                        ( let e = BinOp(_1, Or, _3) in e )
# 356 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 83 "parser.mly"
                        ( let e = BinOp(_1, And, _3) in e )
# 364 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 84 "parser.mly"
                           ( let e = ReadMem(_2) in e )
# 371 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 85 "parser.mly"
                        ( let e = UnOp(Not, _2)  in e)
# 378 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 86 "parser.mly"
                        ( let e = UnOp(Sub, _2)  in e)
# 385 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 87 "parser.mly"
                        ( let e = _2 in e )
# 392 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'num_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 91 "parser.mly"
                             ( _3::_1 )
# 400 "parser.ml"
               : 'num_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 92 "parser.mly"
                             ( [_1] )
# 407 "parser.ml"
               : 'num_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 96 "parser.mly"
                                    (   let l = str_to_byte_list _3 in 
                                        let s = assign_tab _1 l in
                                        wrap s
                                    )
# 418 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'num_list) in
    Obj.repr(
# 100 "parser.mly"
                                        ( let s = assign_tab _1 (List.rev _4) in wrap s )
# 426 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 101 "parser.mly"
                                    ( let s = Assign(_1,_3) in wrap s )
# 434 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 102 "parser.mly"
                                    ( let s = DerefAssign(_2,_4) in wrap s )
# 442 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
                                    ( let s = Label(_1) in wrap s )
# 449 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "parser.mly"
                                    ( let cond = Ast.str_to_cond _1 in let s = Branch(cond, _2) in wrap s )
# 457 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 105 "parser.mly"
                                    ( let s = Cmp(_2, _4) in wrap s )
# 465 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 106 "parser.mly"
                                    ( let s = WriteMem(_2, _5) in wrap s )
# 473 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp_args) in
    Obj.repr(
# 107 "parser.mly"
                                    ( let s = Call(_1, _2) in wrap s )
# 481 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp_args) in
    Obj.repr(
# 108 "parser.mly"
                                    ( let s = ExtCall(_2, _3) in wrap s )
# 489 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 112 "parser.mly"
                        ( _2::_1 )
# 497 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 113 "parser.mly"
                        ( [_1] )
# 504 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                            ( _3::_1 )
# 512 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 118 "parser.mly"
                            ( [_1] )
# 519 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 119 "parser.mly"
                            ( [] )
# 525 "parser.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp_args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 123 "parser.mly"
                                ( _3::_1 )
# 533 "parser.ml"
               : 'exp_args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 124 "parser.mly"
                                ( [_1] )
# 540 "parser.ml"
               : 'exp_args_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                ( [] )
# 546 "parser.ml"
               : 'exp_args_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'args_list) in
    Obj.repr(
# 128 "parser.mly"
                                        ( Args(List.rev _2) )
# 553 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp_args_list) in
    Obj.repr(
# 129 "parser.mly"
                                        ( ExpArgs(List.rev _2) )
# 560 "parser.ml"
               : 'exp_args))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 132 "parser.mly"
                                ( FunBody'(List.rev _2) )
# 567 "parser.ml"
               : 'func_body))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'func_body) in
    Obj.repr(
# 135 "parser.mly"
                                ( let f = Fun'(_2, _3, _4) in wrap f )
# 576 "parser.ml"
               : 'func))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'func_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 139 "parser.mly"
                        ( _2::_1 )
# 584 "parser.ml"
               : 'func_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func) in
    Obj.repr(
# 140 "parser.mly"
                        ( [_1] )
# 591 "parser.ml"
               : 'func_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "parser.mly"
                    ( Prog'([]) )
# 597 "parser.ml"
               : Ast.program'))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'func_list) in
    Obj.repr(
# 144 "parser.mly"
                    ( Prog'(List.rev _1) )
# 604 "parser.ml"
               : Ast.program'))
(* Entry input *)
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
let input (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program')
;;
# 148 "parser.mly"

# 631 "parser.ml"
