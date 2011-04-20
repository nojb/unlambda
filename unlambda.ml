type term =
  | Fun of (term -> term)
  | S
  | K
  | I
  | V

type code =
  | TICK of code * code
  | TERM of term

open Printf

let rec output fmt = function
  | TICK (x, y) -> fprintf fmt "`%a%a" output x output y
  | TERM (Fun _ ) -> fprintf fmt "!"
  | TERM (S) -> fprintf fmt "s"
  | TERM (K) -> fprintf fmt "k"
  | TERM (I) -> fprintf fmt "i"
  | TERM (V) -> fprintf fmt "v"

let rec eval = function
  | TICK (x, y) -> apply (eval x) (eval y)
  | TERM (t) -> t

and apply f x =
  match f with
  | Fun (f) -> f x
  | S -> Fun (fun y -> Fun (fun z -> apply (apply x z) (apply y z)))
  | K -> Fun (fun _ -> x)
  | I -> x
  | V -> V

let parse inch =
  let rec parse_char = function
    | '`' ->
      let f = parse_char (input_char inch) in
      let g = parse_char (input_char inch) in
      TICK (f, g)
    | 's' -> TERM S
    | 'i' -> TERM I
    | 'k' -> TERM K
    | 'v' -> TERM V
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

let natify n =
  let count = ref 0 in
  ignore (apply (apply n (Fun (fun _ -> incr count; V))) V);
  !count

let boolify b =
  let res = ref false in
  ignore (apply (apply b (Fun (fun _ -> res := true; V)))
    (Fun (fun _ -> res := false; V)));
  !res
