open AstNode

exception UndefinedValue
exception InvalidExpr

type value =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool

let sprint_value v =
  match v with
  | Int(i) -> string_of_int i
  | Float(f) -> string_of_float f
  | Str(s) -> s
  | Bool(b) -> if b then "true" else "false"

type env =
  | Env of (string * value) list * env
  | Empty

let rec nearestIdValue env id =
  match env with
  | Empty -> raise UndefinedValue
  | Env(e, next) ->
    try
      let (_, value) = List.find (fun (key, _) -> (key = id)) e in
      value
    with
    Not_found -> nearestIdValue next id

let add_val l r =
  match l with
  | Int(i) -> 
      (match r with 
        | Int(j) -> Int(i + j)
         | Float(j) -> Float(float_of_int(i) +. j)
         | _ -> raise InvalidExpr)
  | Float(i) ->
    (match r with
      | Int(j) -> Float(i +. float_of_int(j))
      | Float(j) -> Float(i +. j)
      | _ -> raise InvalidExpr)
  | _ -> raise InvalidExpr

let minus_val l r =
  match l with
  | Int(i) -> 
      (match r with 
        | Int(j) -> Int(i - j)
         | Float(j) -> Float(float_of_int(i) -. j)
         | _ -> raise InvalidExpr)
  | Float(i) ->
    (match r with
      | Int(j) -> Float(i -. float_of_int(j))
      | Float(j) -> Float(i -. j)
      | _ -> raise InvalidExpr)
  | _ -> raise InvalidExpr

let mul_val l r =
  match l with
  | Int(i) -> 
      (match r with 
        | Int(j) -> Int(i * j)
         | Float(j) -> Float(float_of_int(i) *. j)
         | _ -> raise InvalidExpr)
  | Float(i) ->
    (match r with
      | Int(j) -> Float(i *. float_of_int(j))
      | Float(j) -> Float(i *. j)
      | _ -> raise InvalidExpr)
  | _ -> raise InvalidExpr

let div_val l r =
  match l with
  | Int(i) -> 
      (match r with 
        | Int(j) -> Int(i / j)
         | Float(j) -> Float(float_of_int(i) /. j)
         | _ -> raise InvalidExpr)
  | Float(i) ->
    (match r with
      | Int(j) -> Float(i /. float_of_int(j))
      | Float(j) -> Float(i /. j)
      | _ -> raise InvalidExpr)
  | _ -> raise InvalidExpr


let rec eval_arith a env =
  match a with
  | Plus(op1, op2) -> add_val (eval_arith_op op1 env) (eval_arith_op op2 env)
  | Minus(op1, op2) -> minus_val (eval_arith_op op1 env) (eval_arith_op op2 env)
  | Mul(op1, op2) -> mul_val (eval_arith_op op1 env) (eval_arith_op op2 env)
  | Div(op1, op2) -> div_val (eval_arith_op op1 env) (eval_arith_op op2 env)
and
eval_arith_op op env =
  match op with
  | OpInt(i) -> Int(i)
  | OpFloat(f) -> Float(f)
  | OpIdentifier(id) -> nearestIdValue env id
  | OpArith(a) -> eval_arith a env


let eval (ts: AstNode.t list) =
  let top_env = ref []
  in
  let eval_iter t = 
    match t with
    | Arith(a) -> eval_arith a (Env(!top_env, Empty))
    | _ -> raise InvalidExpr
  in List.map eval_iter ts
