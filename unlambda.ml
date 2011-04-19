type term =
  | Fun of (term -> term)
  | S
  | K
  | I
  | V
  | Dot of char
  | R

type code =
  | TICK of code * code
  | TERM of term
  | D of code

open Printf

let rec output fmt = function
  | TICK (x, y) -> fprintf fmt "`%a%a" output x output y
  | TERM (Fun _ ) -> fprintf fmt "!"
  | TERM (S) -> fprintf fmt "s"
  | TERM (K) -> fprintf fmt "k"
  | TERM (I) -> fprintf fmt "i"
  | TERM (V) -> fprintf fmt "v"
  | TERM (Dot (c)) -> fprintf fmt ".%c" c
  | TERM (R) -> fprintf fmt "r"
  | D (x) -> fprintf fmt "`d%a" output x

let rec eval = function
  | TICK (x, y) -> apply (eval x) (eval y)
  | TERM (t) -> t
  | D (x) -> Fun (fun y -> apply (eval x) y)

and apply f x =
  match f with
  | Fun (f) -> f x
  | S -> Fun (fun y -> Fun (fun z -> apply (apply x z) (apply y z)))
  | K -> Fun (fun _ -> x)
  | I -> x
  | V -> V
  | Dot c -> (print_char c; flush stdout; x)
  | R -> (print_newline (); x)

let parse inch =
  let rec parse_char = function
    | '`' ->
        begin match input_char inch with
        | 'd' -> D (parse_char (input_char inch))
        | _ as c ->
            let f = parse_char c in
            let g = parse_char (input_char inch) in
            TICK (f, g)
        end
    | 's' -> TERM S
    | 'i' -> TERM I
    | 'k' -> TERM K
    | 'v' -> TERM V
    | '.' -> TERM (Dot (input_char inch))
    | 'r' -> TERM R
    | ' ' | '\n' | '\t' ->
        parse_char (input_char inch)
    | '#' ->
        let rec loop = function
          | '\n' -> parse_char (input_char inch)
          | _ -> loop (input_char inch)
        in loop (input_char inch)
    | _ as c ->
        Printf.eprintf "ignoring char: %c\n%!" c;
        parse_char (input_char inch)
  in parse_char (input_char inch)
