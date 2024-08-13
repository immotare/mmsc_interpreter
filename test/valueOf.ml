exception UnsupportedValue
let unwrapIntValue (a: Eval.value)  = 
  match a with
  | Eval.Int(i) -> i
  | _ -> raise UnsupportedValue


let unwrapFloatValue (a: Eval.value)  = 
  match a with
  | Eval.Float(f) -> f 
  | _ -> raise UnsupportedValue
