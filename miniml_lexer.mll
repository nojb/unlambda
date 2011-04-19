{
  open Miniml_parser
}

rule token = parse
  | ['\n''\t'' ']
  { token lexbuf }
  | "is_zero"
  { IS_ZERO }
  | ';'
  { SEMI }
  | '+'
  { PLUS }
  | "**"
  { TIMESTIMES }
  | '('
  { LPAREN }
  | ')'
  { RPAREN }
  | "rec"
  { REC }
  | "true"
  { TRUE }
  | "false"
  { FALSE }
  | "let"
  { LET }
  | '='
  { EQ }
  | "in"
  { IN }
  | "fun"
  { FUN }
  | "->"
  { ARROW }
  | "if"
  { IF }
  | "then"
  { THEN }
  | "else"
  { ELSE }
  | "print_int"
  { PRINT_INT }
  | ['a'-'z']['a'-'z''0'-'9''_']* as ident
  { IDENT ident }
  | ['0'-'9']+ as int
  { INT (int_of_string int) }
  | eof
  { EOF }
