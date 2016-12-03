module Main where

import Types
import Parser
import Lexer
import Interpreter

main :: IO ()
main = do
  prog <- getContents
  run . parseComplex . lexComplex $ prog
