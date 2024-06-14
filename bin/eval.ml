open AstNode

exception UndefinedValue
exception InvalidExpr
exception OutOfRangeIndex

type value =
  | Int of int
  | Float of float
  | Str of string
  | Bool of bool
  | Proc of params * expr list

let sprint_value v =
  match v with
  | Int(i) -> string_of_int i
  | Float(f) -> string_of_float f
  | Str(s) -> s
  | Bool(b) -> if b then "true" else "false"
  | Proc(_) -> "Proc"

let bool_of_value v =
  match v with
  | Bool(b) -> b
  | _ -> true

type frame = (string * value) list

type env =
  | Env of frame * env
  | Empty

let print_env env =
  let rec iter e =
    match e with
    | Env(f, ne) -> print_string("--------------------------------");
                    print_newline();
                    List.iter (fun (k, v) -> Printf.printf "id:%s, value:%s\n" k (sprint_value v)) f;
                    print_string("--------------------------------");
                    print_newline();
                    iter ne
    | Empty -> print_string "."
  in iter env

let rec nearest_id_value env id =
  match env with
  | Empty -> raise UndefinedValue
  | Env(e, next) ->
    try
      let (_, value) = List.find (fun (key, _) -> (key = id)) e in
      value
    with
    Not_found -> nearest_id_value next id

let append_entry_into_frame env entry idx =
  let rec iter iter_env iter_idx =
    match env with
    | Env(f, nf) -> 
        if iter_idx = idx then
          Env(entry::f, nf)
        else
          iter iter_env (iter_idx+1)
    | Empty -> raise OutOfRangeIndex
  in
  iter env 0




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
  | OpIdentifier(id) -> nearest_id_value env id
  | OpArith(a) -> eval_arith a env


let value_of_atom t env =
  match t with
  | Mint(i) -> Int(i)
  | Mfloat(f) -> Float(f)
  | Mstr(s) -> Str(s)
  | Midentifier(id) -> nearest_id_value env id
  | Mbool(b) -> Bool(b)


let eval (ts: AstNode.t list) =
  let ge = Env([], Empty)
  in
  let rec eval_iter t env = 
    match t with
    | Atom(a) ->
      (value_of_atom a env, env)
    | Arith(a) -> 
        ((eval_arith a env), env)
    | Define(a) -> 
        eval_define a env
    | If(pred, a, b) ->
        eval_if pred a b env
    | Pred(a) ->
        eval_pred a env
    | Lambda(a) ->
        eval_lambda a env
    | ProcCall(a) ->
        eval_proc_call a env
    (* | _ -> raise InvalidExpr *)
  and
  eval_define d env =
    match d with
    | Bind(id, expr) -> 
        let (v, ne) = eval_iter expr env in
        (Str(id), append_entry_into_frame ne (id, v) 0)
    | Func(_, _, _) ->
      raise InvalidExpr
  and
  eval_if pred a b env =
  let (value, ne) = eval_iter pred env in
  if bool_of_value value then
    eval_iter a ne
  else
    eval_iter b ne 
  and
  eval_pred a env =
  match a with
  | And(p1, p2) -> 
    let (v1, _) = eval_iter p1 env in
    let (v2, _) = eval_iter p2 env in
    (Bool((bool_of_value v1) && (bool_of_value v2)), env)
  | Or(p1, p2) ->
    let (v1, _) = eval_iter p1 env in
    let (v2, _) = eval_iter p2 env in
    (Bool((bool_of_value v1) || (bool_of_value v2)), env)
  | _ -> raise InvalidExpr
  and
  eval_lambda a env =
  let (params, exprs) = a in
  (Proc(params, exprs), env)
  and
  eval_proc_call a env =
  let (proc, lexical_params) = a in
  (* params_valueはlexical_paramsを評価したvalueの配列 *)
  let params_value = (
    let env_ref = ref env in
    List.map (fun expr -> (let (v, ne) = eval_iter expr !env_ref in env_ref := ne; v)) lexical_params
  )
  in
    match proc with 
    | Atom(a) -> (
        let atom_value = value_of_atom a env in
        match atom_value with
        | Proc(params, body) ->
          (apply_proc params params_value body env, env)
        | _ -> raise InvalidExpr
      )
    | Lambda(a) ->
      let (params, body) = a in
      (apply_proc params params_value body env, env) 
    | _ -> raise InvalidExpr
  and
  apply_proc params params_value body env =
  if (List.length params = List.length params_value) then
    let new_frame = List.combine params params_value in
    let env_ref = ref (Env(new_frame, env)) in
    let result = List.map
                  (fun expr -> 
                    (let (v, ne) = eval_iter expr !env_ref
                                   in env_ref := ne;
                                   v)) body in
    List.hd (List.rev result)
  else
    raise InvalidExpr
  (* tsは新しいノードが先頭にきているので後ろから評価*)
  in List.fold_right (fun nt e -> 
                      (let (v, ne) = eval_iter nt e in
                      let vs = sprint_value v in
                      Printf.printf "value:%s" vs;
                      print_newline();
                      ne)) ts ge