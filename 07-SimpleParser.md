# Overview

The goal is to write a simple parser for our tiny programming language called _nano_,
that looks like this:
```
def f(x,y)=x+2*y

let y=f(3,4)
let z=y+3*f(y,3)

print z
```

Each line of nano is one of the following:
 - a function definition using keyword `def`
 - a variable assignment using keyword `let`
 - a print statement using keyword `print`

The expressions consist of variables and integer literals,
`+` and `*` operators,
and function calls.

We first develop a _SimpleParser_ library that helps us develop the actual parser for the programming language _nano_.
To keep the things as simple as possible,
the parser works fine on valid inputs,
but it does not provide the best error messages on invalid inputs.


# Preliminaries

```haskell
module SimpleParser where

import Control.Applicative (Alternative(..), some, (<*), (*>))
import Data.Bifunctor (first)
import Data.Char (isDigit, isLetter)
```

### The `Parser` datatype

The `Parser` type is actually a `newtype` that keeps a pointer to a function `parse`.
Type `Parser` is parametrised with a parameter with a type parameter `a` that denotes the result of the parse.
The `parse` function takes a `String` as the input and returns either
a `Left errMsg` if it cannot parse the input or
a `Right (result, rest)` where `result` is the successfully parsed value and
`rest` is the remainder of the input.

```haskell
newtype Parser a = Parser{ parse :: String -> Either String (a, String) }
```

The idea is to have multiple parsers for some basic building blocks (e.g., parsing number, variable names, ...)
and compose them to build parsers for more complicated strcutures (e.g., function definitions)
and eventually the whole program.

### `runParser`
`runParser` takes a parser and an input string and runs the parser on the input.
It makes sure that the whole input is consumed.

```haskell
runParser :: Parser a -> String -> Either String a
runParser (Parser parse) input = 
  case parse input of
    Left msg -> Left msg
    Right (x, "") -> Right x
    Right (_, r) -> Left $ "Parser did not consume the entire input " ++ r
```

### `charMatch`
`charMatch` is a parser that matches against a single character is the given condition is `True`.

```haskell
charMatch :: (Char -> Bool) -> Parser Char
charMatch f = Parser charParser
  where charParser (c:r) | f c = Right (c, r)
                         | otherwise = Left "Predicated does not match"
        charParser "" = Left "Empty input"
```

# `Functor`, `Applicative` and `Alternative` instances

```haskell
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
```

# Helper functions

Recall that `Alternative` the following default definitions for `many` and `some`:

```haskell
many v = some v <|> pure []
some v = (:) <$> v <*> many v
```

The following helper functions provide building blocks for more complicated parsers.

```haskell
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
```

# A parser for `nano`

## Required types
``` haskell
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
```


## The parser itself
```haskell
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
```


## :ledger: Homework
Extend the _nano_ parser so that it is able to parse binary and hex numbers. Binary numbers start with a `0b` (e.g., `0b1101010`) and hex numbers start with a `0x` (e.g., `0x1f3ab`). Hex numbers only use lowercase [`a`..`f`] and `x` letters.

## :ledger: Homework
The parser currently does not handle whitespaces. E.g., ` let  y = f ( 3, 4 ) ` seems to be a legit statement. Modify the parser so that it handles whitespaces before and after keywords, variable names, function names, operators and numbers. Think about an approach that is both clean and has minimal changes to the source code.

## :ledger: Homework (Advanced)
Update the parser so that it reports better error messages including column and line number.


