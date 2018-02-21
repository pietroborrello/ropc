# 1 "lexer.mll"
 
    open Ast
    open Common
    open Parser
    open Printf
    open Scanf
    open Lexing


    let keyword_table = 
    create_hashtable 8 [
      ("fun", FUN);
      ("cmp", CMP);
    ]

    let incr_linenum lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
          pos_lnum = pos.pos_lnum + 1;
          pos_bol = pos.pos_cnum;
        }

# 25 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\225\255\226\255\227\255\228\255\001\000\230\255\231\255\
    \232\255\233\255\234\255\235\255\236\255\237\255\238\255\239\255\
    \240\255\241\255\242\255\243\255\244\255\245\255\246\255\247\255\
    \248\255\003\000\079\000\163\000\255\000\079\001\089\001\099\001\
    \122\001\007\000\253\255\252\255\177\001\250\255\193\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\026\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\029\000\006\000\006\000\029\000\000\000\000\000\255\255\
    \001\000\255\255\255\255\255\255\004\000\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\038\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\000\000\038\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\004\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\006\000\025\000\005\000\024\000\037\000\020\000\028\000\
    \012\000\011\000\016\000\018\000\013\000\017\000\034\000\015\000\
    \030\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\000\000\000\000\000\000\014\000\000\000\000\000\
    \023\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\008\000\000\000\007\000\022\000\000\000\
    \000\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\027\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\010\000\021\000\009\000\019\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\000\000\000\000\000\000\000\000\026\000\000\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\035\000\000\000\000\000\
    \000\000\000\000\000\000\037\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\000\000\000\000\
    \001\000\255\255\026\000\255\255\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
    \033\000\033\000\033\000\033\000\033\000\033\000\033\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\255\255\000\000\000\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\035\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\000\000\000\000\000\000\000\000\
    \036\000\000\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\025\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\255\255\255\255\255\255\026\000\255\255\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
    \026\000\026\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\255\255\255\255\
    \255\255\255\255\255\255\038\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\255\255\255\255\
    \000\000\005\000\027\000\025\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
    \028\000\028\000\028\000\028\000\028\000\028\000\028\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\031\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\031\000\031\000\031\000\031\000\
    \031\000\031\000\032\000\032\000\032\000\032\000\032\000\032\000\
    \032\000\032\000\032\000\032\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\038\000\255\255\255\255\031\000\031\000\031\000\031\000\
    \031\000\031\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\030\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\032\000\032\000\032\000\032\000\032\000\
    \032\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \036\000\255\255\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 30 "lexer.mll"
                inum
# 243 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 31 "lexer.mll"
    ( 
        let num = int_of_string inum in
        (* printf "integer: %s (%d)\n" inum num; *)
        NUM num
    )
# 251 "lexer.ml"

  | 1 ->
let
# 36 "lexer.mll"
                         hnum
# 257 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 37 "lexer.mll"
    (
        let num = Scanf.sscanf hnum "0x%x" (fun x->x)  in
        (* printf "hex integer: %s (%d)\n" hnum num; *)
        NUM num
    )
# 265 "lexer.ml"

  | 2 ->
let
# 42 "lexer.mll"
                                     qc
# 271 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos (lexbuf.Lexing.lex_start_pos + 3) in
# 43 "lexer.mll"
    (
        let c = String.get qc 1 in
        let num = Char.code c in
        NUM num
    )
# 279 "lexer.ml"

  | 3 ->
let
# 48 "lexer.mll"
                label
# 285 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 49 "lexer.mll"
    (
        let label = String.sub label 0 (String.length label - 1) in
        (* printf "label: %s\n" label;*)
        LABEL label
    )
# 293 "lexer.ml"

  | 4 ->
let
# 54 "lexer.mll"
                branch
# 299 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 55 "lexer.mll"
    (
        let cond = String.sub branch 1 (String.length branch - 1) in
        (*let f x = Printf.printf "%s," x in
        let _ = List.map f Ast.branches in*)
        if List.exists (fun x -> x = cond) Ast.branches then
            BRANCH cond
        else
            ID branch
    )
# 311 "lexer.ml"

  | 5 ->
let
# 64 "lexer.mll"
                         str
# 317 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 65 "lexer.mll"
    (
        let s = String.sub str 1 (String.length str - 1) in
        STR s
    )
# 324 "lexer.ml"

  | 6 ->
let
# 69 "lexer.mll"
            word
# 330 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 70 "lexer.mll"
    ( try
        let token = Hashtbl.find keyword_table word in
        (*printf "keyword: %s\n" word;*)
        token
      with Not_found ->
        (*printf "identifier: %s\n" word;*)
        ID word
    )
# 341 "lexer.ml"

  | 7 ->
# 78 "lexer.mll"
          ( DOLLAR )
# 346 "lexer.ml"

  | 8 ->
# 79 "lexer.mll"
          ( AT )
# 351 "lexer.ml"

  | 9 ->
# 80 "lexer.mll"
          ( XOR )
# 356 "lexer.ml"

  | 10 ->
# 81 "lexer.mll"
          ( OR )
# 361 "lexer.ml"

  | 11 ->
# 82 "lexer.mll"
          ( AND )
# 366 "lexer.ml"

  | 12 ->
# 83 "lexer.mll"
          ( NOT )
# 371 "lexer.ml"

  | 13 ->
# 84 "lexer.mll"
          ( PLUS )
# 376 "lexer.ml"

  | 14 ->
# 85 "lexer.mll"
          ( MINUS )
# 381 "lexer.ml"

  | 15 ->
# 86 "lexer.mll"
          ( MUL )
# 386 "lexer.ml"

  | 16 ->
# 87 "lexer.mll"
          ( DIV )
# 391 "lexer.ml"

  | 17 ->
# 88 "lexer.mll"
          ( EQ )
# 396 "lexer.ml"

  | 18 ->
# 89 "lexer.mll"
          ( COMMA )
# 401 "lexer.ml"

  | 19 ->
# 90 "lexer.mll"
          ( LPAREN )
# 406 "lexer.ml"

  | 20 ->
# 91 "lexer.mll"
          ( RPAREN )
# 411 "lexer.ml"

  | 21 ->
# 92 "lexer.mll"
          ( LCURLY )
# 416 "lexer.ml"

  | 22 ->
# 93 "lexer.mll"
          ( RCURLY )
# 421 "lexer.ml"

  | 23 ->
# 94 "lexer.mll"
          ( LBRACKET )
# 426 "lexer.ml"

  | 24 ->
# 95 "lexer.mll"
          ( RBRACKET )
# 431 "lexer.ml"

  | 25 ->
# 96 "lexer.mll"
          ( BANG )
# 436 "lexer.ml"

  | 26 ->
# 99 "lexer.mll"
    ( 
        token lexbuf 
    )
# 443 "lexer.ml"

  | 27 ->
# 103 "lexer.mll"
    (
        incr_linenum lexbuf;
        token lexbuf
    )
# 451 "lexer.ml"

  | 28 ->
# 108 "lexer.mll"
    ( 
        token lexbuf 
    )
# 458 "lexer.ml"

  | 29 ->
let
# 111 "lexer.mll"
           c
# 464 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 112 "lexer.mll"
    ( 
        printf "Unrecognized character: %c\n" c;
        token lexbuf 
    )
# 471 "lexer.ml"

  | 30 ->
# 117 "lexer.mll"
    ( 
        EOF
    )
# 478 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

