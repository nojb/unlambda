{
  open Miniml_parser
}

rule token = parse
  | ['\n''\t'' ']
  { token lexbuf }
  | ';'
  { SEMI }
  | "is_zero"
  { IS_ZERO }
  | '-'
  { MINUS }
  | '+'
  { PLUS }
  | "**"
  { TIMESTIMES }
  | '*'
  { TIMES }
  | '('
  { LPAREN }
  | ')'
  { RPAREN }
  | '['
  { LBRACK }
  | ']'
  { RBRACK }
  | "::"
  { COLONCOLON }
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
  | "&&"
  { AND }
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
  | ['a'-'z']['a'-'z''0'-'9''_']* as ident
  { IDENT ident }
  | ['0'-'9']+ as int
  { INT (int_of_string int) }
  | eof
  { EOF }
