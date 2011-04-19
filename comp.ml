let compile inch outch =
  Unlambda.output outch
    (Lambda.unlambda
      (Lambda.abstract
        (Miniml.compile
          (Miniml_parser.prog Miniml_lexer.token
            (Lexing.from_channel inch))))) in

let compile1 inch outch =
  Lambda.output outch
    (Miniml.compile
      (Miniml_parser.prog Miniml_lexer.token
        (Lexing.from_channel inch))) in

let eval inch =
  Unlambda.eval
    (Lambda.unlambda
      (Lambda.abstract
        (Miniml.compile
          (Miniml_parser.prog Miniml_lexer.token
            (Lexing.from_channel inch)))))
in
  ignore (eval stdin)
  (* Printf.fprintf stdout "%t\n%!" (compile stdin) *)
