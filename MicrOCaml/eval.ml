open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

let rec eval_expr env e =
match e with
|Value v -> v
|ID(n) -> lookup env n
|Not(expr) -> (match eval_expr env expr with 
  |Bool true -> Bool false
  |Bool false -> Bool true 
  |_ -> raise (TypeError "Expected type bool")) 
|Binop(op, expr1, expr2) -> 
  let ev1 = eval_expr env expr1 in
  let ev2 = eval_expr env expr2 in
  (match op with 
  |Concat -> (match ev1 with
    |String s1 -> (match ev2 with
      |String s2 -> String (s1 ^ s2)
      |_ -> raise (TypeError "Expected type string"))
    |_ -> raise (TypeError "Expected type string"))
  |Equal -> (match ev1 with
    |String s1 -> (match ev2 with
      |String s2 -> if s1 = s2 then Bool true else Bool false
      |_ -> raise (TypeError "Cannot compare types"))
    |Int i1 -> (match ev2 with
      |Int i2 -> if i1 = i2 then Bool true else Bool false
      |_ -> raise (TypeError "Cannot compare types"))
    |Bool b1 -> (match ev2 with
      |Bool b2 -> if b1 = b2 then Bool true else Bool false
      |_ -> raise (TypeError "Cannot compare types"))
    |_ -> raise (TypeError "Cannot compare types"))
  |NotEqual -> eval_expr env (Not(Binop(Equal, expr1, expr2)))
  |Or -> (match ev1 with
    |Bool b1 -> (match ev2 with
      |Bool b2 -> Bool (b1 || b2)
      |_ -> raise (TypeError "Expected type bool")) 
    |_ -> raise (TypeError "Expected type bool"))
  |And -> (match ev1 with
    |Bool b1 -> (match ev2 with
      |Bool b2 -> Bool (b1 && b2)
      |_ -> raise (TypeError "Expected type bool")) 
    |_ -> raise (TypeError "Expected type bool"))
  |_ -> (match ev1 with
    |Int i1 -> (match ev2 with
      |Int i2 -> (match op with
        |Add -> Int (i1 + i2)
	|Sub -> Int (i1 - i2)
        |Mult -> Int (i1 * i2)
	|Div -> if i2 = 0 then raise DivByZeroError else Int (i1 / i2)
        |Greater -> Bool (i1 > i2)
	|Less -> Bool (i1 < i2)
        |GreaterEqual -> Bool (i1 >= i2)
	|LessEqual -> Bool (i1 <= i2)
        |_ -> raise (TypeError "This should never happen"))
      |_ -> raise (TypeError "Expected type int"))
    |_ -> raise (TypeError "Expected type int")))
|If (guard, tbranch, fbranch) ->
  (match eval_expr env guard with
  |Bool b -> if b then (eval_expr env tbranch) else (eval_expr env fbranch)
  |_ -> raise (TypeError "Expected type bool"))
|Let (this_id, is_rec, i_state, body) -> 
  if is_rec = false then
    let v = eval_expr env i_state in
    let new_env = extend env this_id v in
    eval_expr new_env body
  else
    let new_env = extend_tmp env this_id in
    let v = eval_expr new_env i_state in
    update new_env this_id v;
    eval_expr new_env body
|Fun (param, body) -> Closure (env, param, body)
|FunctionCall (expr1, expr2) ->
  (match eval_expr env expr1 with
  |Closure (new_env, this_var, expr) -> 
    let v = eval_expr env expr2 in
    eval_expr (extend new_env this_var v) expr   
  |_ -> raise (TypeError "Not a function"))

    
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)

let eval_mutop env m = 
match m with
|Def (v, expr) ->  
  let new_env = extend_tmp env v in
  let new_val = eval_expr new_env expr in
  update new_env v new_val;
  (new_env, Some new_val) 
|Expr expr ->
  let v = eval_expr env expr in
  (env, Some v)
|NoOp -> (env, None)
