module Types where

import Data.Map.Strict (Map)
import Data.HashMap.Strict (HashMap)
import Linear.V2

data Token =
  VarT String |
  IntT Integer |
  JT |
  PlusT |
  MinusT |
  PrintT |
  SetT |
  IfT |
  NewlineT
  deriving Show

type Program = Map LineNumber Instruction
--type Line = (LineNumber, Instruction)
type LineNumber = V2 Integer
data Instruction =
  Print [Expression] |
  Set Variable Expression |
  If Expression Instruction
  deriving Show
type Variable = String
data Expression =
  Integer Integer |
  Variable Variable |
  Expression :+ Expression |
  Expression :- Expression
  deriving Show
type Registry = HashMap Variable Integer

