module Nano.Evaluator where

import Nano.Types


eval :: [Command] -> [Int] -- returns a list of printed messages, they are all Ints, right?
eval commands = evalWithContext [] [] commands

evalWithContext :: [(FuncName, ([VarName], Exp))] -> [(VarName, Int)] -> [Command] -> [Int]
evalWithContext _ _ [] = []
evalWithContext functions variables (cmd:rest) =
    case cmd of
        Def name params body -> evalWithContext ((name, (params, body)):functions) variables rest
        Let name rhs -> evalWithContext functions ((name, evalExpression functions variables rhs):variables) rest
        Print exp -> evalExpression functions variables exp : evalWithContext functions variables rest

evalExpression :: [(FuncName, ([VarName], Exp))] -> [(VarName, Int)] -> Exp -> Int
evalExpression functions variables exp =
    case exp of
        Num n -> n
        Var name -> lookupBinding variables name
        Sum e1 e2 -> evalExpression functions variables e1 + evalExpression functions variables e2
        Mult e1 e2 -> evalExpression functions variables e1 * evalExpression functions variables e2
        Call name params ->
            let (paramNames, body) = lookupBinding functions name
                paramValues = map (evalExpression functions variables) params
            in  evalExpression functions (zip paramNames paramValues ++ variables) body


lookupBinding :: [(String, a)] -> String -> a
lookupBinding [] name = error $ "Variable or function not found: " ++ name
lookupBinding ((key, val):rest) name | key == name = val
                                     | otherwise = lookupBinding rest name
