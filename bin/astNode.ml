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

type params = identifier list

type expr =
  | Arith of arith
  | Atom of atom
  | Lambda of lambda_stmt
  | Define of define_stmt
  | Pred of pred
  | If of if_stmt
  and 
  define_stmt =
  | Bind of identifier * expr
  | Func of identifier * params * (expr list)
  and
  lambda_stmt = params * (expr list)
  and pred = 
  | And of expr * expr
  | Or of expr * expr
  | Gt of expr *  expr
  | Lt of expr * expr
  | Eq of expr * expr
  and
  if_stmt = pred * expr * expr

type t = expr

module PPrint: sig
  val sprint_node : t -> string
end = struct

  type ppnode =
    | Node of string * ppnode list
    | Leaf of string
  
  let make_indent n = String.make n ' '

  let rec sprint_ppnodes p indent =
    match p with
    | Node(s, ps) ->
      Printf.sprintf "%s%s\n%s"
                    (make_indent indent)
                    s 
                    (
                      List.fold_left
                      (fun acc a -> acc ^ (if acc <> "" then "\n" else "") ^ sprint_ppnodes a (indent+2))
                      ""
                      ps
                    )
    | Leaf(s) -> (make_indent indent) ^ s


  let atom_to_string a =
    match a with
      | Mint(i) ->  "Int:" ^ string_of_int(i)
      | Mfloat(f) -> "Float:" ^ string_of_float(f)
      | Mstr(s) -> "String:" ^ s
      | Midentifier(id) -> "Identifier:" ^ id

  let rec arith_op_to_ppnode a =
    match a with
      | OpInt(i) -> Leaf("Int:" ^ string_of_int(i))
      | OpFloat(f) -> Leaf("Float:" ^ string_of_float(f))
      | OpIdentifier(id) -> Leaf("Identifier:" ^ id)
      | OpArith(a) -> Node("Arith:", [arith_to_ppnode a])
    and arith_to_ppnode a =
      match a with
      | Plus(op1, op2) -> 
        Node("Plus:", [(arith_op_to_ppnode op1); (arith_op_to_ppnode op2)])
      | Minus(op1, op2) -> 
        Node("Minus:", [(arith_op_to_ppnode op1); (arith_op_to_ppnode op2)])
      | Mul(op1, op2) -> 
        Node("Mul:", [(arith_op_to_ppnode op1); (arith_op_to_ppnode op2)])
      | Div(op1, op2) -> 
        Node("Div:", [(arith_op_to_ppnode op1); (arith_op_to_ppnode op2)])

  let params_to_string (a: params) =
    List.fold_left (fun acc param -> acc ^ " " ^ param) "" a

  let rec to_ppnode e =
    match e with
    | Atom(a) -> Leaf(atom_to_string a)
    | Arith(a) -> arith_to_ppnode a
    | Define(a) -> define_to_ppnode a
    | Lambda(a) -> lambda_to_ppnode a
    | _ -> Leaf("")
    and
    define_to_ppnode a =
    match a with
    | Bind(id, expr) ->
      Node("Define:", 
          [
            Leaf(Printf.sprintf "Identifier:%s" id);
            Node("Expr:", [to_ppnode expr])])
    | Func(id, params, exprs) ->
      Node("Define:",
          [
            Leaf(Printf.sprintf "Identifier:%s" id);
            Leaf(Printf.sprintf "Params:%s" (params_to_string params));
            Node("Exprs:", List.map (fun a -> to_ppnode a) exprs)])
    and
    lambda_to_ppnode a =
    let (params, exprs) = a
    in 
    Node("Lambda:", [
      Leaf(Printf.sprintf "Params:%s" (params_to_string params));
      Node("Exprs:", List.map (fun a -> to_ppnode a) exprs)
    ])

    let sprint_node e = sprint_ppnodes (to_ppnode e) 0
end
