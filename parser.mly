%{
open Syntax
%}
%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT ANDAND OROR EQ
%token IF THEN ELSE LET IN ANDLIT TRUE FALSE
%token FUN RARROW

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    Expr SEMISEMI { Exp $1 }
  | LetDecl SEMISEMI { LetDecl $1 }

Expr :
    IfExpr { $1 }
  | LetExpr { $1 }
  | BExpr { $1 }
  | FunExpr { $1 }

LetDecl :
    LET Binding { $2 }
  | LET Binding LetDecl { List.append $2 $3 }

BExpr :
    BExpr ANDAND LTExpr  { BinOp (And, $1, $3) }
  | BExpr OROR LTExpr   { BinOp (Or, $1, $3) }
  | LTExpr { $1 }

LTExpr :
    PExpr LT PExpr { BinOp (Lt, $1, $3) }
  | PExpr { $1 }

PExpr :
    PExpr PLUS MExpr { BinOp (Plus, $1, $3) }
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

IfExpr :
    IF Expr THEN Expr ELSE Expr { IfExp ($2, $4, $6) }

LetExpr :
  LET Binding IN Expr { LetExp ($2, $4) }

FunExpr :
    FUN ID RARROW Expr { FunExp ($2, $4) }

Binding :
    Equality ANDLIT Binding { $1::$3 }
  | Equality { [$1] }

Equality :
    ID EQ Expr { ($1, $3) }
