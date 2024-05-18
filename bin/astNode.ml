type identifier = string

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

type expr =
  | Arith of arith
  | Atom of atom

type params = identifier list
type define_stmt =
  | Bind of identifier * expr
  | Func of identifier * params * (expr list)

type lambda_stmt = params * (expr list)

type t = 
  | Atom of atom
  | Arith of arith
  | Define of define_stmt
  | If
  | Lambda of lambda_stmt

module PPrint = struct
  let atom_to_string (a: atom) =
    match a with
      | Mint(i) -> "AtomInt:" ^ string_of_int(i)
      | Mfloat(f) -> "AtomFloat:" ^ string_of_float(f)
      | Mstr(s) -> "AtomString:" ^ s
      | Midentifier(id) -> "AtomIdentifier:" ^ id

  let rec arith_op_to_string (a:arith_op) =
    match a with
      | OpInt(i) -> "OpInt:" ^ string_of_int(i)
      | OpFloat(f) -> "OpFloat:" ^ string_of_float(f)
      | OpIdentifier(id) -> "OpIdentifier:" ^ id
      | OpArith(a) -> "OpArith:\n  " ^ arith_to_string a
    and arith_to_string (a: arith) =
    match a with
      | Plus(op1, op2) -> Printf.sprintf "Plus:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
      | Minus(op1, op2) -> Printf.sprintf "Minus:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
      | Mul(op1, op2) -> Printf.sprintf "Mul:\n  Op1:%s\n  Op2:%s\n" (arith_op_to_string op1) (arith_op_to_string op2)
      | Div(op1, op2) -> Printf.sprintf 
                         "Div:\n  Op1:%s\n  Op2:%s\n"
                         (arith_op_to_string op1)
                         (arith_op_to_string op2)

  let params_to_string (a: params) =
    List.fold_left (fun acc param -> acc ^ " " ^ param) "" a

  let define_to_string (a:define_stmt) =
    let s =
      match a with
      | Bind(id, _) -> Printf.sprintf "Bind:\n\tId:%s\n\tExpr:()" id
      | Func(id, params, _) -> Printf.sprintf 
                                  "Func:\n\tId:%s\n\tParams:%s\n\tExpr:()"
                                  id
                                  (params_to_string params)
      in "Define:\n" ^ s

  let lambda_to_string (a: lambda_stmt) =
    let (params, _) = a
    in
    Printf.sprintf "Lambda:\n\tParams:%s\n\tExpr:()" (params_to_string params) 

  let to_string t =
    match t with
      | Atom(a) -> atom_to_string a
      | Arith(a) -> arith_to_string a
      | Define(a) -> define_to_string a
      | Lambda(a) -> lambda_to_string a
      | _ -> ""
end
