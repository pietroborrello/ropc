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

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program'
