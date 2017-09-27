module Nano.Parser where

import Control.Applicative (Alternative(..), (<*))
import ParserLib.SimpleParser
import Nano.Types


atom, term, expression :: Parser Exp
atom = Call <$> name <*> parens (commaSep expression)
    <|> Var <$> name
    <|> Num <$> number
    <|> parens expression
term = Mult <$> atom <* string "*" <*> term <|> atom
expression = Sum <$> term <* string "+" <*> expression <|> term

command :: Parser Command
command =
       pure Def <* string "def " <*> name <*> parens (commaSep name) <* string "=" <*> expression
   <|> pure Let <* string "let " <*> name <* string "=" <*> expression
   <|> pure Print <* string "print " <*> expression

program :: Parser [Command]
program = sepBy newline command <* many newline
