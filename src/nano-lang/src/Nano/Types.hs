module Nano.Types where

type VarName = String
type FuncName = String

data Exp = Num Int
         | Var VarName
         | Sum Exp Exp
         | Mult Exp Exp
         | Call FuncName [Exp] deriving Show

data Command = Def FuncName [VarName] Exp
             | Let VarName Exp
             | Print Exp deriving Show

