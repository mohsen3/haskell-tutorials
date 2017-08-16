```haskell
module SimpleParser where

import Control.Applicative (Alternative(..), some, (<*), (*>))
import Data.Bifunctor (first)
import Data.Char (isDigit, isLetter)

newtype Parser a = Parser{ parse :: String -> Either String (a, String) } -- either err message or success + rest of stream

runParser :: Parser a -> String -> Either String a
runParser (Parser parse) input = 
  case parse input of
    Left msg -> Left msg
    Right (x, "") -> Right x
    Right (_, r) -> Left $ "Parser did not consume the entire input " ++ r

charMatch :: (Char -> Bool) -> Parser Char
charMatch f = Parser charParser
  where charParser (c:r) | f c = Right (c, r)
                         | otherwise = Left "Predicated does not match"
        charParser "" = Left "Empty input"

instance Functor Parser where
  fmap f (Parser parse) = Parser $ fmap (first f) . parse 

instance Applicative Parser where
  pure x = Parser $ \s -> Right (x, s)
  Parser p1 <*> Parser p2 = Parser app
    where app s = case p1 s of
                    Left msg -> Left msg
                    Right (f, r) -> fmap (first f) (p2 r)

instance Alternative Parser where
  empty = Parser $ const (Left "Alternative.empty")
  (Parser p1) <|> (Parser p2) =
    Parser $ \s -> case (p1 s, p2 s) of
      (Right x, _) -> Right x
      (_, Right x) -> Right x
      (Left msg1, Left msg2) -> Left $ concat ["Alternative failed: ", msg1, " <|> ", msg2]

char :: Char -> Parser Char
char ch = charMatch (==ch)

string :: String -> Parser ()
string "" = Parser $ \s -> Right ((), s)
string (c:r) = char c *> string r

number :: Parser Int
number = read <$> some digit
  where digit = charMatch isDigit

name :: Parser String
name = some (charMatch isLetter)

parens :: Parser a -> Parser a
parens x = char '(' *> x <* char ')'

sep :: Parser s -> Parser a -> Parser [a]
sep s x = (:) <$> x <*> many (s *> x)

commaSep :: Parser a -> Parser [a]
commaSep = sep (char ',')
```


```haskell
type VarName = String
type FuncName = String
data Exp = Num Int | Var VarName | Sum [Exp] | Mult [Exp] deriving Show
data Command = FunctionDef FuncName [VarName] Exp | FunctionCall FuncName [Exp] deriving Show

atom = Var  <$> name <|> Num  <$> number <|> parens expression
term = Mult <$> sep (string "*") atom
expression = Sum <$> sep (string "+") term

command :: Parser Command
command =  string "def " *> (pure FunctionDef <*> name <*> parens (commaSep name) <* string "=" <*> expression)
       <|> (FunctionCall <$> name <*> parens (commaSep expression))

```
