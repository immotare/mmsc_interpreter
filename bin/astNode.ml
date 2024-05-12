type atom =
  | Mint of int
  | Mfloat of float
  | Mstr of string
  | Midentifier of string

type arith =
  | Plus of arith_op * arith_op
  | Minus of arith_op * arith_op
  | Mul of arith_op * arith_op
  | Div of arith_op * arith_op
  and  arith_op =
  | OpInt of int
  | OpFloat of float
  | OpIdentifier of string
  | OpArith of arith

type keyword =
  | Define
  | If
  | Lambda

type t = 
  | Atom of atom
  | Arith of arith
  | Keyword of keyword

let atom_to_string (a: atom) =
  match a with
    | Mint(i) -> "AtomInt:" ^ string_of_int(i)
    | Mfloat(f) -> "AtomFloat:" ^ string_of_float(f)
    | Mstr(s) -> "AtomString:" ^ s
    | Midentifier(id) -> "AtomIdentifier:" ^ id

let rec arith_op_to_string (a:arith_op) =
  match a with
    | OpInt(i) -> "OpInt:\n  " ^ string_of_int(i)
    | OpFloat(f) -> "OpFloat:\n  " ^ string_of_float(f)
    | OpIdentifier(id) -> "OpIdentifier:\n  " ^ id
    | OpArith(a) -> "OpArith:\n  " ^ arith_to_string a
  and arith_to_string (a: arith) =
  match a with
    | Plus(op1, op2) -> Printf.sprintf "Plus:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
    | Minus(op1, op2) -> Printf.sprintf "Minus:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
    | Mul(op1, op2) -> Printf.sprintf "Mul:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
    | Div(op1, op2) -> Printf.sprintf "Div:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)

let keyword_to_string (a: keyword) =
  match a with
    | Define -> "Keyword: define"
    | If -> "Keyword: if"
    | Lambda -> "Keyword: lambda"

let to_string t =
  match t with
    | Atom(a) -> atom_to_string(a)
    | Arith(a) -> arith_to_string(a)
    | Keyword(a) -> keyword_to_string(a)
