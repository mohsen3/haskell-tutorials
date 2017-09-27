module Main where

import ParserLib.SimpleParser (runParser)
import Nano.Parser (program)


main :: IO ()
main = do
  source <- readFile "program.nano"
  print $ runParser program source
