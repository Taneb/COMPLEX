{
module Parser (parseComplex) where

import Types
import Data.Map.Strict (fromList)
import Linear.V2
}

%name parseComplex
%tokentype { Token }
%error { parseError }

%token
	var     { VarT $$ }
	int     { IntT $$ }
	J       { JT }
	'+'     { PlusT }
        '-'     { MinusT }
	Print   { PrintT }
	Set     { SetT }
	If      { IfT }
	newline { NewlineT }

%%

Program : Program1 { fromList $1 }

Program1 : {- empty -}  { [] }
         | Program1 Line { $2 : $1 }

Line : LineNumber Instruction newline { ( $1 , $2 ) }

LineNumber : int               { V2 $1 0 }
           | int J             { V2 0 $1 }
	   | int '+' int J     { V2 $1 $3 }
           | '-' int           { V2 (negate $2) 0 }
	   | '-' int J         { V2 0 (negate $2) }
	   | int '-' int J     { V2 $1 (negate $3) }
	   | '-' int '+' int J { V2 (negate $2) $4 }
           | '-' int '-' int J { V2 (negate $2) (negate $4) }

Instruction : Print ExpressionList      { Print $2 }
            | Set var Expression   { Set $2 $3 }
	    | If Expression Instruction { If $2 $3 }

ExpressionList : Expression ExpressionList { $1 : $2 }
	       | {- empty -}               { [] }

Expression : int                       { Integer $1 }
           | var                       { Variable $1 }
	   | Expression '+' Expression { $1 :+ $3 }
	   | Expression '-' Expression { $1 :- $3 }

{
parseError :: [Token] -> a
parseError _ = error "PARSE ERROR"
}
