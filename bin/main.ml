let () = 
  Printexc.print(fun () ->
    try
      let lexbuf = Lexing.from_channel stdin in
      let node_cnt = ref 0 in
      while true do
        let result = Parser.main Lexer.token lexbuf in
        print_string "-------------------------";
        print_newline();
        Printf.printf "<%d>" !node_cnt;
        print_newline();
        print_string (AstNode.PPrint.sprint_node result);
        print_newline();
        print_string "-------------------------";
        print_newline();
        print_newline();
        node_cnt := !node_cnt + 1;
      done
    with
      Lexer.EOF -> exit 0
  )()
