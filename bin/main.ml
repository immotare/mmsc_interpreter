let () = 
  Printexc.print(fun () ->
    try
      let lexbuf = Lexing.from_channel stdin in
      while true do
        let result = Parser.main Lexer.token lexbuf in
        print_string (AstNode.PPrint.to_string result); print_newline()
      done
    with
      Lexer.EOF -> exit 0
  )()
