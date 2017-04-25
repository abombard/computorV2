let main () =
    let buf = Lexing.from_channel stdin in
    try
        let rec aux () =
            Parser.main Lexer.token buf; aux ()
        in aux ()
    with
    | Lexer.Error msg -> Printf.eprintf "%s%!" msg
    | Parser.Error -> Printf.eprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start buf)

let _ = main ()
