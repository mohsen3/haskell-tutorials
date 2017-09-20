module SimpleParser where

import Control.Applicative (Alternative(..), some, (<*), (*>))
import Data.Bifunctor (first)
import Data.Char (isDigit, isLetter)

newtype Parser a = Parser{ parse :: String -> Either String (a, String) }

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
  Parser p1 <|> Parser p2 =
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

sepBy :: Parser s -> Parser a -> Parser [a]
sepBy s x = (:) <$> x <*> many (s *> x)

commaSep :: Parser a -> Parser [a]
commaSep = sepBy (char ',')

newline :: Parser ()
newline = string "\r\n" <|> string "\n"

--------------------------------------------------------
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

------------------------------------------------------
main = do
  source <- readFile "program.nano"
  print $ runParser program source
