(* ML interpreter / type reconstruction *)

type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp = 
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp 
  | LetExp of id * exp * exp
  | LetRecExp of id * id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
;;

type program = 
    Exp of exp
  | LetDecl of (id * exp) list
  | LetRecDecl of id * id * exp
;;

type tyvar = int

type ty = 
    TyInt 
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
;;

let rec string_of_ty = function
    TyInt   -> "int"
  | TyBool  -> "bool"
  | TyVar x -> "t" ^ string_of_int x
  | TyFun (a, b) -> "(" ^ string_of_ty a ^ "->" ^ string_of_ty b ^ ")" 
;;

let pp_ty ty = print_string (string_of_ty ty)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1;
    v
    in body
;;

let rec freevar_ty = function
    TyVar x -> MySet.singleton x
  | TyFun (a, b) -> MySet.unite (freevar_ty a) (freevar_ty b)
  | _ -> MySet.void
