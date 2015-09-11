open Syntax

type value =
    IntV of int
  | BoolV of bool
  | Error of string
;;

let err msg =
  Error msg
;;

let pp_val = function
    IntV i  -> Printf.printf "int = %d\n"  i
  | BoolV b -> Printf.printf "bool = %b\n" b
  | Error e -> Printf.printf "Error: %s\n" e
;;

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
;;

let rec eval_exp env = function
    Var x ->
      (try Environment.lookup x env with
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
      (match test with
        BoolV true  -> eval_exp env exp2
      | BoolV false -> eval_exp env exp3
      | _ -> err ("Test expression must be boolean: if")) 
;;

let eval_decl env = function
  Exp e -> let v = eval_exp env e in ("-", env, v) 
;;
