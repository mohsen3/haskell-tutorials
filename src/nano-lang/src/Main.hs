module Main where

import ParserLib.SimpleParser (runParser)
import Nano.Parser (program)
import Nano.Evaluator

main :: IO ()
main = do
  source <- readFile "program.nano"
  case runParser program source of
    Left msg -> print $ "error: " ++ msg
    Right commands -> print $ eval commands
