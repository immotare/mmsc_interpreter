{
  open Parser
  exception EOF
}

let space = [' ' '\t']
let break = ['\n' '\r' ';']
let nonzero = ['1'-'9']
let zero = '0'
let digit = ('-' | '+')? (zero | nonzero (zero | nonzero)*)

let float = ('-' | '+')? (zero | digit) '.' digit

let alphabet = ['a'-'z''A'-'Z']
let identifier = alphabet (alphabet | zero | nonzero)*

let str = '"' [^'"']* '"'

rule token = parse
  | space { token lexbuf }
  | break { Lexing.new_line lexbuf; token lexbuf }
  | digit as i { INT(int_of_string i) }
  | float as f { FLOAT(float_of_string f) }
  | str as s { STR(s) }
  | "#f" { BOOL(false) }
  | "#t" { BOOL(true) }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MUL }
  | '/' { DIV }
  | "and" { AND }
  | "or" { OR }
  | ">" { GT }
  | "<" { LT }
  | "=" { EQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "define" { DEFINE }
  | "if" { IF }
  | "lambda" { LAMBDA }
  | identifier as id { IDENTIFIER(id) }
  | eof { raise EOF }
  | _ as c { Printf.printf "unexpected character: %C" c; token lexbuf }
