%{
open Syntax
%}
%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR EQ
%token IF THEN ELSE LET IN TRUE FALSE

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LET ID EQ Expr SEMISEMI { LetDecl($2, $4) }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | BExpr { $1 }

BExpr :
    BExpr AND LTExpr  { BinOp (And, $1, $3) }
  | BExpr OR LTExpr   { BinOp (Or, $1, $3) }
  | LTExpr { $1 }

LTExpr :
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
  | MExpr { $1 }

MExpr :
    MExpr MULT AExpr { BinOp (Mult, $1, $3) }
  | AExpr { $1 }

AExpr :
    INTV { ILit $1 }
  | TRUE { BLit true }
  | FALSE { BLit false }
  | ID { Var $1 }
  | LPAREN Expr RPAREN { $2 }

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
  LET ID EQ Expr IN Expr { LetExp ($2, $4, $6) }
