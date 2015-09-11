open Syntax
open Util

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
and dnval = exval
;;

exception Error of string

let err msg = raise (Error msg)

let pp_val = function
    IntV i  -> Printf.printf "int = %d"  i
  | BoolV b -> Printf.printf "bool = %b" b
  | ProcV (id, body, env) -> Printf.printf "fun %s -> ..." id
;;

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
;;

let rec eval_exp env = function
    Var x ->
      (try Environment.lookup x env with
        Environment.Not_bound -> err ("Unbound variable " ^ x))
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
  | LetExp (bind, body) ->
      let rec eval env = function
          [] -> eval_exp env body
        | (x,e)::rest ->
            let v = eval_exp env e in
            let env = Environment.extend x v env in
            eval env rest      
      in
      eval env bind
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | AppExp (exp1, exp2) ->
      let funval  = eval_exp env exp1 in
      let arg     = eval_exp env exp2 in
      (match funval with
          ProcV (id, body, env') ->
            let newenv = Environment.extend id arg env' in
              eval_exp newenv body
        | _ -> err ("Non-function value is applied"))

;;

let values env pairs =
  let rec f alist = function
    [] -> alist
    | (x,_)::rest ->
        let v = Environment.lookup x env in
        f (assoc_set x v alist) rest
  in
  f [] pairs
;;

let eval_decl env = function
    Exp e -> 
      let v = eval_exp env e in 
      ([("-", v)], env) 
  | LetDecl pairs ->
      let extend = fun env (id,e) -> 
        Environment.extend id (eval_exp env e) env in
      let newenv = fold_left extend env pairs in
      (values newenv pairs, newenv)
;;
