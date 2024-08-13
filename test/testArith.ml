open ValueOf

let evalAndUnwrapIntArithStr arith_str =
  let lexbuf = Lexing.from_string arith_str in
  let parse_result = Parser.main Lexer.token lexbuf in
  let result_values, _ = Eval.eval [parse_result] in
  let result_unwrapped = List.map (fun (a) -> unwrapIntValue a) result_values in
  result_unwrapped

let evalAndUnwrapFloatArithStr arith_str =
  let lexbuf = Lexing.from_string arith_str in
  let parse_result = Parser.main Lexer.token lexbuf in
  let result_values, _ = Eval.eval [parse_result] in
  let result_unwrapped = List.map (fun (a) -> unwrapFloatValue a) result_values in
  result_unwrapped


let test_add_int () = 
  let arith_str = "(+ 3 4)" in
  let unwrapped_result_list = evalAndUnwrapIntArithStr arith_str in 
  Alcotest.(check (list int)) "equal" [7] unwrapped_result_list 


let test_minus_int () = 
  let arith_str = "(- 3 4)" in
  let unwrapped_result_list = evalAndUnwrapIntArithStr arith_str in 
  Alcotest.(check (list int)) "equal" [-1] unwrapped_result_list

let test_mul_int () = 
  let arith_str = "(* 3 4)" in
  let unwrapped_result_list = evalAndUnwrapIntArithStr arith_str in 
  Alcotest.(check (list int)) "equal" [12] unwrapped_result_list

let test_div_int () =
  let arith_str = "(/ 4 2)" in
  let unwrapped_result_list = evalAndUnwrapIntArithStr arith_str in 
  Alcotest.(check (list int)) "equal" [2] unwrapped_result_list

let test_arith_nested () = 
  let arith_str = "(+ (- 3 4) (* (/ 4 1) 5))" in
  let unwrapped_result_list = evalAndUnwrapIntArithStr arith_str in 
  Alcotest.(check (list int)) "equal" [19] unwrapped_result_list


let test_float_add () =
  let arith_str = "(+ 3.4 -7)" in
  let unwrapped_result_list = evalAndUnwrapFloatArithStr arith_str in
  let delta = 0.01 in
  Alcotest.(check (list (float delta))) "equal" [-3.6] unwrapped_result_list


let () =
  let open Alcotest in
  run "Utils" [
    "arith_simple_int", [
      test_case "add" `Quick test_add_int;
      test_case "minus" `Quick test_minus_int;
      test_case "mul" `Quick test_mul_int;
      test_case "div" `Quick test_div_int;
    ];
    "arith_nested_int", [
      test_case "nested" `Quick test_arith_nested;
    ];
    "arith_simple_float", [
      test_case "add" `Quick test_float_add;
    ]
  ]