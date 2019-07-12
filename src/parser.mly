%{
open Syntax
open Util

let f_plus = FunExp ("+l", FunExp ("+r", BinOp (Plus, Var "+l", Var "+r")))
let f_mult = FunExp ("*l", FunExp ("*r", BinOp (Mult, Var "*l", Var "*r")))
let f_lt   = FunExp ("<l", FunExp ("<r", BinOp (Lt,   Var "<l", Var "<r")))
let f_and  = FunExp ("&l", FunExp ("&r", BinOp (And,  Var "&l", Var "&r")))
let f_or   = FunExp ("|l", FunExp ("|r", BinOp (Or,   Var "|l", Var "|r")))

let fold_args args body =
  fold_right (fun x body -> FunExp (x, body)) body args
;;

let fold_argsd args body =
  fold_right (fun x body -> DFunExp (x, body)) body args

let fold_let bind body =
  fold_right (fun (x,v) body -> LetExp (x, v, body)) body bind
%}
%token LPAREN RPAREN SEMISEMI
%token PLUS MINUS MULT LT ANDAND OROR EQ
%token IF THEN ELSE LET REC IN ANDLIT TRUE FALSE
%token DFUN FUN RARROW

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LetDecl SEMISEMI { LetDecl $1 }
  | LetRecDecl SEMISEMI { $1 }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | LetRecExpr { $1 }
  | BExpr { $1 }
  | UExpr { $1 }
  | FunExpr { $1 }

LetDecl :
    LET Binding { $2 }
  | LET Binding LetDecl { List.append $2 $3 }

LetRecDecl :
    LET REC ID EQ FUN ID RARROW Expr {
      LetRecDecl ($3, $6, $8)
    }

BExpr :
    BExpr ANDAND LTExpr  { BinOp (And, $1, $3) }
  | BExpr OROR LTExpr   { BinOp (Or, $1, $3) }
  | LTExpr { $1 }

UExpr :
    MINUS LTExpr { UniOp (Negate, $2) }

LTExpr :
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | PExpr MINUS MExpr { BinOp (Minus, $1, $3) }
  | MExpr { $1 }

MExpr :
    MExpr MULT AppExpr { BinOp (Mult, $1, $3) }
  | AppExpr { $1 }

AppExpr :
    AppExpr AExpr { AppExp ($1, $2) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }
  | LPAREN Infix RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
  LET Binding IN Expr { LetExp ($2, $4) }

LetRecExpr :
    LET REC ID EQ FUN ID RARROW Expr IN Expr {
      LetRecExp ($3, $6, $8, $10)
    }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) }
  | FUN Arguments RARROW Expr { fold_args $2 $4 }
  | DFUN ID RARROW Expr { DFunExp ($2, $4) }
  | DFUN Arguments RARROW Expr { fold_argsd $2 $4 }

Infix :
    PLUS    { f_plus }
  | MULT    { f_mult }
  | LT      { f_lt }
  | ANDAND  { f_and }
  | OROR    { f_or }

Binding :
    Equality ANDLIT Binding { $1::$3 }
  | Equality { [$1] }

Equality :
    ID Arguments EQ Expr { ($1, fold_args $2 $4) }
  | ID EQ Expr { ($1, $3) }

Arguments :
    ID Arguments { $1::$2 }
  | ID { [$1] }
