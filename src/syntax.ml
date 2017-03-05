(* ML interpreter / type reconstruction *)

type id = string

type binOp = Plus | Minus | Mult | Lt | And | Or

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

type program =
    Exp of exp
  | LetDecl of (id * exp) list
  | LetRecDecl of id * id * exp
