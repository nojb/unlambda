module M = Map.Make (String)

let lambda inch outch =
  Lambda.print_lambda
    (Miniml.compile
      (Miniml_parser.prog Miniml_lexer.token
        (Lexing.from_channel inch)))

let compile inch =
  Miniml.compile
    (Miniml_parser.prog Miniml_lexer.token
      (Lexing.from_channel inch))

let () =
  let x = compile stdin in
  Format.printf "%a\n%!"
    Lambda.pr_lambda x;
  let y = Lambda.eval M.empty x in
  Format.printf "%d\n%!" (Lambda.natify y)
