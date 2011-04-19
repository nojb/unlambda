%{
  open Miniml
%}

%token SEMI IS_ZERO
%token REC
%token PRINT_INT TIMESTIMES
%token LPAREN RPAREN TRUE FALSE EOF
%token LET EQ IN FUN ARROW IF THEN ELSE
%token PLUS
%token <string> IDENT
%token <int> INT

%left letprec
%left SEMI
%left PLUS
%left TIMESTIMES

%type <Miniml.exp> prog
%start prog

%%

prog:
    exp EOF
  { $1 }
  ;

simple_exp:
    LPAREN exp RPAREN
  { $2 }
  | INT
  { Int $1 }
  | IDENT
  { Var $1 }
  | TRUE
  { True }
  | FALSE
  { False }
  ;

exp:
    simple_exp
  { $1 }
  | LET IDENT EQ exp IN exp %prec letprec
  { Let ($2, $4, $6) }
  | LET REC IDENT LPAREN RPAREN EQ exp IN exp %prec letprec
  { Rec ($3, ([], $7), $9) }
  | LET REC x = IDENT xs = nonempty_list (IDENT) EQ y = exp IN z = exp %prec letprec
  { Rec (x, (xs, y), z) }
  | IS_ZERO simple_exp
  { IsZero $2 }
  | PRINT_INT simple_exp
  { PrintInt $2 }
  | simple_exp LPAREN RPAREN
  { App ($1, []) }
  | x = simple_exp xs = nonempty_list (simple_exp)
  { App (x, xs) }
  | FUN LPAREN RPAREN ARROW y = exp
  { Lam ([], y) }
  | FUN xs = nonempty_list (IDENT) ARROW y = exp
  { Lam (xs, y) }
  | IF exp THEN exp ELSE exp
  { If ($2, $4, $6) }
  | simple_exp PLUS simple_exp
  { Add ($1, $3) }
  | simple_exp TIMESTIMES simple_exp
  { Pow ($1, $3) }
  | exp SEMI exp
  { Seq ($1, $3) }
  ;
