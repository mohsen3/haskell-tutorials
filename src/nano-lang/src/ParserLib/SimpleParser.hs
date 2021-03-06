module ParserLib.SimpleParser where

import Control.Applicative (Alternative(..), some)
import Data.Bifunctor (first)
import Data.Char (isDigit, isLetter)

-- We only use pattern matching to extract the function, so we don't need to use record syntax
newtype Parser a = Parser (String -> Either String (a, String))

runParser :: Parser a -> String -> Either String a
runParser (Parser parse) input =
  case parse input of
    Left msg -> Left msg
    Right (x, "") -> Right x
    Right (_, r) -> Left $ "Parser did not consume the entire input " ++ r

charMatch :: (Char -> Bool) -> Parser Char
charMatch f = Parser charParser
  where charParser (c:r) | f c = Right (c, r)
                         | otherwise = Left $ "Predicate does not match at: " ++ head (lines (c:r))
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

-- We don't really need the monad instance in our project since
-- the applicative instance is powerful enough for our purpose.
-- i.e., you can comment this out and the project compiles!
instance Monad Parser where
  return = pure
  -- Parser a >>= (a -> Parser b) -> Parser b
  (Parser p1) >>= f = Parser g
    where
      -- g :: String -> Either String (b, String)
      g s = do
        (a, r1) <- p1 s
        let (Parser p2) = f a
        p2 r1

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
sepBy s x = (:) <$> x <*> many (s *> x) <|> pure []

commaSep :: Parser a -> Parser [a]
commaSep = sepBy (char ',')

newline :: Parser ()
newline = string "\r\n" <|> string "\n"
