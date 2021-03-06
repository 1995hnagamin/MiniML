{
let reservedWords = [
  ("else" , Parser.ELSE) ;
  ("false", Parser.FALSE) ;
  ("if"   , Parser.IF) ;
  ("then" , Parser.THEN) ;
  ("true" , Parser.TRUE) ;
  ("let"  , Parser.LET) ;
  ("rec"  , Parser.REC) ;
  ("in"   , Parser.IN);
  ("and"  , Parser.ANDLIT);
  ("fun"  , Parser.FUN);
  ("dfun" , Parser.DFUN);
]
}

rule main = parse
  [' ' '\009' '\012' '\n']+ { main lexbuf }
| "-"? ['0'-'9']+
  {Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "("     { Parser.LPAREN }
| ")"     { Parser.RPAREN }
| ";;"    { Parser.SEMISEMI }
| "+"     { Parser.PLUS }
| "-"     { Parser.MINUS }
| "*"     { Parser.MULT }
| "<"     { Parser.LT }
| "&&"    { Parser.ANDAND }
| "||"    { Parser.OROR }
| "="     { Parser.EQ }
| "->"    { Parser.RARROW }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '~']*
  { let id = Lexing.lexeme lexbuf in
      try
          List.assoc id reservedWords
      with
          _ -> Parser.ID id
  }
| eof { exit 0 } 

