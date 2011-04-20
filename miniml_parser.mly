%{
  open Miniml
%}

%token IS_ZERO
%token REC TIMES
%token TIMESTIMES MINUS
%token LPAREN RPAREN TRUE FALSE EOF
%token LET EQ IN FUN ARROW IF THEN ELSE
%token PLUS
%token <string> IDENT
%token <int> INT

%left PLUS MINUS
%left TIMES
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
  | LET IDENT EQ exp IN exp
  { Let ($2, $4, $6) }
  | LET IDENT LPAREN RPAREN EQ exp IN exp
  { Let ($2, Lam ([], $6), $8) }
  | LET x = IDENT xs = nonempty_list (IDENT) EQ y = exp IN z = exp
  { Let (x, Lam (xs, y), z) }
  | LET REC IDENT LPAREN RPAREN EQ exp IN exp
  { Rec ($3, ([], $7), $9) }
  | LET REC x = IDENT xs = nonempty_list (IDENT) EQ y = exp IN z = exp
  { Rec (x, (xs, y), z) }
  | IS_ZERO simple_exp
  { IsZero $2 }
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
  | exp PLUS exp
  { Add ($1, $3) }
  | exp TIMES exp
  { Mul ($1, $3) }
  | exp MINUS exp
  { Sub ($1, $3) }
  | exp TIMESTIMES exp
  { Pow ($1, $3) }
  ;
