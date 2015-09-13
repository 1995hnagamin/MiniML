open Syntax

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
    Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err "Argument must be integer: +")
  | Mult -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
              | _ -> err "Argument must be integer: *")
  | Lt   -> (match ty1, ty2 with
                TyInt, TyInt -> TyBool
              | _ -> err "Argument must be integer: +")
  | And  -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err "Argument must be boolean: &&")
  | Or   -> (match ty1, ty2 with
                TyBool, TyBool -> TyBool
              | _ -> err "Argument must be boolean: ||")
;;

let rec ty_exp tyenv = function
    Var x ->
      (try Environment.lookup x tyenv with
          Environment.Not_bound -> err ("Unbound variable: " ^ x))
  | ILit _ -> TyInt
  | BLit _ -> TyBool
  | BinOp (op, exp1, exp2) ->
      let tyarg1 = ty_exp tyenv exp1 in
      let tyarg2 = ty_exp tyenv exp2 in
      ty_prim op tyarg1 tyarg2
  | IfExp (exp1, exp2, exp3) ->
      let ty1 = ty_exp tyenv exp1 in
      let ty2 = ty_exp tyenv exp2 in
      let ty3 = ty_exp tyenv exp3 in
      if (ty1 != TyBool) 
      then err "Test expression must be boolean: if"
      else if (ty2 != ty3) 
        then err "then/else clauses must have the same type: if"
      else ty2
  | LetExp (id, exp1, exp2) ->
      let ty1 = ty_exp tyenv exp1 in
      let tyenv' = Environment.extend id ty1 tyenv in
      ty_exp tyenv' exp2
  | _ -> err "Not implemented"
;;

let ty_decls tyenv = function
    Exp e -> [("-", ty_exp tyenv e)]
  | _ -> err "Not implemented"
;;
