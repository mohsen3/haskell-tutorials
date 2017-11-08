module Main where

import Control.Applicative
import Control.Monad
import ParserLib.SimpleParser

parseSum :: Parser Int
parseSum = do
  x <- number
  op <- char '+' <|> char '-' 
  y <- number
  -- guard $ x < y
  return (if op =='+' then x + y else x - y)

main :: IO ()
main = do
  let r = runParser parseSum "123+111"
  print r

