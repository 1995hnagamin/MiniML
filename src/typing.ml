open Syntax
open Util

exception Error of string

let err s = raise (Error s)

type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let eqs_of_subst = map (fun (x, a) -> (TyVar x, a))

let subst_type s t =
  let rec subst_ty t (source, target) = match t with
      TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (a, b) ->
      TyFun (subst_ty a (source, target), subst_ty b (source, target))
    | TyList a ->
      TyList (subst_ty a (source, target))
    | TyVar x ->
        if x = source then target else (TyVar x)
  in
  fold_left subst_ty t s

let map_subst x a =
  let s = [(x, a)] in
  map (fun (t, u) -> (subst_type s t, subst_type s u))

let rec unify : (ty * ty) list -> subst = function
  [] -> []
    | (p, q)::rest -> match (p, q) with
      (TyFun (a, b), TyFun (a', b')) -> unify ((a, a')::(b, b')::rest)
    | (TyVar x, a) ->
        if a = TyVar x
          then unify rest
        else if MySet.elem x (freevar_ty a)
        then err "error"
        else
          (x, a)::(unify (map_subst x a rest))
    | (a, TyVar x) -> unify ((TyVar x, a)::rest)
    | _ ->
        if p = q
          then unify rest
        else
          err "error"

let ty_prim op ty1 ty2 = match op with
    Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Minus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
  | Lt   -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
  | And  -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
  | Or   -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)

let ty_prim_unary op ty = match op with
    Negate -> ([ty, TyInt], TyInt)

let ty_if ty1 ty2 ty3 = ([(ty1, TyBool); (ty2, ty3)], ty2)

let ty_app ty1 ty2 =
  match ty1 with
    TyFun (a, b) -> ([(a, ty2)], b)
  | TyVar x ->
      let a = TyVar (fresh_tyvar ()) in
      let b = TyVar (fresh_tyvar ()) in
      ([(ty1, TyFun (a, b)); (a, ty2)], ty2)
  | _ -> err "error"

let rec ty_exp tyenv = function
    Var x ->
      (try ([], Environment.lookup x tyenv) with
          Environment.Not_bound -> err ("Unbound variable: " ^ x))
  | ILit _ -> ([], TyInt)
  | BLit _ -> ([], TyBool)
  | BinOp (op, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_prim op ty1 ty2 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
      let s3 = unify eqs in
      (s3, subst_type s3 ty)
  | UniOp (op, exp) ->
      let (s, ty) = ty_exp tyenv exp in
      let (eqs, ty) = ty_prim_unary op ty in
      let eqs = (eqs_of_subst s) @ eqs in
      let s = unify eqs in
      (s, subst_type s ty)
  | IfExp (exp1, exp2, exp3) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (s3, ty3) = ty_exp tyenv exp3 in
      let (eqs4, ty) = ty_if ty1 ty2 ty3 in
      let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ eqs4 in
      let s = unify eqs in
      (s, subst_type s ty)
  | LetExp (id, exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let tyenv' = Environment.extend id ty1 tyenv in
      ty_exp tyenv' exp2
  | FunExp (id, exp) ->
      let domty = TyVar (fresh_tyvar ()) in
      let tyenv' = Environment.extend id domty tyenv in
      let (s, ranty) = ty_exp tyenv' exp in (* G,id:domty |- exp:ranty *)
      (s, TyFun (subst_type s domty, ranty))
  | AppExp  (exp1, exp2) ->
      let (s1, ty1) = ty_exp tyenv exp1 in
      let (s2, ty2) = ty_exp tyenv exp2 in
      let (eqs3, ty) = ty_app ty1 ty2 in
      let s = unify ((eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3) in
      (s, subst_type s ty)
  | LetRecExp (id, para, exp1, exp2) ->
      let domty = TyVar (fresh_tyvar ()) in
      let ranty = TyVar (fresh_tyvar ()) in
      let tyenv2 = Environment.extend id (TyFun (domty, ranty)) tyenv in
      let tyenv1 = Environment.extend para domty tyenv2 in
      let (s1, ty1) = ty_exp tyenv1 exp1 in
      let (s2, ty2) = ty_exp tyenv2 exp2 in
      let (eqs3, ty) = ([(ranty, ty1)], ty2) in
      let s = unify ((eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3) in
      (s, subst_type s ty)

let ty_letdecl tyenv (id, exp) =
  let (_, ty) = ty_exp tyenv exp in
  let tyenv' = Environment.extend id ty tyenv in
  tyenv'

let ty_letdecls tyenv binds =
  let tyenv' = fold_left ty_letdecl tyenv binds in
  Environment.resolve tyenv' binds

let ty_letrecdecl tyenv id para exp =
  let lrexp = LetRecExp (id, para, exp, Var id) in
  let (_, ty) = ty_exp tyenv lrexp in
  [(id, ty)]

let ty_decls tyenv = function
    Exp e ->
      let (_, ty) = ty_exp tyenv e in
      [("-", ty)]
  | LetDecl binds -> ty_letdecls tyenv binds
  | LetRecDecl (id, para, exp) -> ty_letrecdecl tyenv id para exp
