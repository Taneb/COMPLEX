{
module Lexer (lexComplex) where

import Types (Token(..))
}

%wrapper "basic"

$upper = [A-Z]
$digit = [0-9]

tokens :-
  PRINT {const PrintT}
  SET {const SetT}
  IF {const IfT}
  J {const JT}
  \+ {const PlusT}
  \- {const MinusT}
  \n {const NewlineT}
  $digit+ {IntT . read}
  $upper+ {VarT}
  [\ \t\f\v\r]+ ;


{
lexComplex :: String -> [Token]
lexComplex = alexScanTokens
}
