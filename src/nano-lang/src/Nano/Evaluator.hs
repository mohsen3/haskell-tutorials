module Nano.Evaluator where

import Nano.Types


eval :: [Command] -> [Int] -- returns a list of printed messages, they are all Ints, right?
eval commands = evalWithContext [] [] commands

evalWithContext :: [(FuncName, ([VarName], Exp))] -> [(VarName, Int)] -> [Command] -> [Int]
evalWithContext _ _ [] = []
evalWithContext functions variables (cmd:rest) =
    case cmd of
        Def name params body -> undefined -- add it to the functions list and call evalWithContext recursively
        Let name rhs -> undefined -- add it to the variables list and call evalWithContext recursively
        Print exp -> undefined -- eval expression and add it to the return list

evalExpression :: [(FuncName, ([VarName], Exp))] -> [(VarName, Int)] -> Exp -> Int
evalExpression functions variables exp =
    case exp of
        Num n -> n
        Var name -> lookupVar variables name
        Sum e1 e2 -> evalExpression functions variables e1 + evalExpression functions variables e2
        Mult e1 e2 -> evalExpression functions variables e1 * evalExpression functions variables e2
        Call name params -> undefined -- find the param names and then evaluate body of the function
                                      -- using global variable list plus the following
                                      -- `zipWith (evalExpression functions variables) paramNames params`


lookupVar :: [(VarName, Int)] -> VarName -> Int
lookupVar [] name = error $ "Variable not found " ++ name
lookupVar ((key, val):rest) name | key == name = val
                                 | otherwise = lookupVar rest name
