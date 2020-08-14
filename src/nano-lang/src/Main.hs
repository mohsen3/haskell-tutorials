module Main where

import Control.Applicative
import Control.Monad
import ParserLib.SimpleParser

import Nano.Parser(program)
import Nano.Evaluator(eval)

main :: IO ()
main = do
  programText <- readFile "program.nano"
  print programText
  let parsed = runParser program programText
  print parsed
  print $ case parsed of
            Right cmds -> show $ eval cmds
            Left err -> "Err -> " ++ err

