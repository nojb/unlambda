let compile inch outch =
  Unlambda.output outch
    (Lambda.unlambda
      (Lambda.abstract
        (Miniml.compile
          (Miniml_parser.prog Miniml_lexer.token
            (Lexing.from_channel inch))))) in

let lambda inch outch =
  Lambda.print_lambda
    (Miniml.compile
      (Miniml_parser.prog Miniml_lexer.token
        (Lexing.from_channel inch))) in

let eval inch =
  Unlambda.eval
    (Lambda.unlambda
      (Lambda.abstract
        (Miniml.compile
          (Miniml_parser.prog Miniml_lexer.token
            (Lexing.from_channel inch))))) in

let unlambda inch =
    (Lambda.unlambda
      (Lambda.abstract
        (Miniml.compile
          (Miniml_parser.prog Miniml_lexer.token
            (Lexing.from_channel inch)))))
in
  lambda stdin stdout;
  (* let x = unlambda stdin in
  print_endline "about to eval";
  print_int (Unlambda.natify (Unlambda.eval x));
  print_newline () *)
  (* ignore (eval stdin) *)
(*  Printf.fprintf stdout "%t\n%!" (compile stdin) *)
