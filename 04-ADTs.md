
# Algebraic data types

The simplest form

```haskell

data Bool = True | False

data DayOfWeek =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

```

Constructors can have fields

```haskell
data Person = Person String Int String

data Tree = Internal Tree Int Tree | Leaf Int

data LinkedList = Cons Int LinkedList | Nil

data Colour = Black | Red | Green | Blue | Cyan | Yellow | Magenta | White | 
              RGB Int Int Int |
              RGBA Int Int Int Int |
              CMYK Float Float Float Float

data Shape = Circle Double |
             Rectangle Double Double |
             Square Double |
             Triangle Double Double

```

Data types can be parameterized

```haskell
data Tree a = Internal (Tree a) a (Tree a) | Leaf a

data LinkedList a = Cons a (LinkedList a) | Nil
```

## Pattern matching on ADTs

```haskell
sum' :: LinkedList ->  Int
sum' (Cons i r) = i + sum' r
sum' Nil = 0
```

```haskell
area :: Shape -> Double
area (Circle r) = 3.14 * r * r
area (Rectangle a b) = a * b
area (Square a) = a * a
area (Triangle h r) = 0.5 * r * h
```

## Constructors are functions
Constructors are similar to functions

```haskell
λ> data LinkedList = Cons Int LinkedList | Nil
λ> :t Cons
Cons :: Int -> LinkedList -> LinkedList
λ> :t Nil
Nil :: LinkedList
```

```haskell
data Imaginary = Im { img :: Int, real :: Int }
numbers = zipWith Im [1, 2, 3] [5, 6, 7]
fromReal = Im 0
```


## `Maybe` and `Either`

```haskell
data Maybe a = Nothing | Just a

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
```

```haskell
data Either a b = Left a | Right b

data Date = Date {year :: Int, month :: Int, day :: Int} deriving (Eq, Show)
mkDate :: Int -> Int -> Int -> Either String Date
mkDate y m d
  | d > 31 || d < 1 = Left ("Incorrect day " ++ show d)
  | m > 12 || m < 1 = Left ("Unknown month " ++ show m)
  | otherwise       = Right $ Date y m d
```

## Record syntax

```haskell
data Tree = Internal{ left :: Tree, data_ :: Int, right :: Tree } | Leaf { data_ :: Int }
```

```haskell
data Person = Person{ name :: String, age :: Int, address :: String }

p = Person "Mike" 10 "Vancouver, Candada" -- you can still use the positional syntax
p2 = p{ age = 11 } -- record update
mike'sAge = age p2 -- age is a function of Person -> Int

```

## The `deriving` keyword

```haskell
data DayOfWeek =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                      deriving (Eq, Ord, Show, Read, Enum, Bounded)
```

## Data constructors can be operators

```haskell
data List a = Empty | a ::: List a deriving (Eq, Ord, Show, Read)
infixr 1 :::

list = 1 ::: 2 ::: 3 ::: Empty
```

### :ledger: Homework

Consider the following definition of `Colour`:

```haskell
data Colour = White | Black | Red | Green | Blue |
              RGB Int Int Int |
              CMYK Float Float Float Float
```

Define the following functions

  - `toRGB :: Colour -> (Int, Int, Int)` that converts a `Colour` object to a tuple of red, green, and blue colours.
     Use the formula suggested in [this link](http://www.rapidtables.com/convert/color/cmyk-to-rgb.htm) for CMYK.
  - `fromRGB :: (Int, Int, Int) -> Maybe Colour` that converts the given RGB tuple into a `Colour` object.
    Return `Nothing` if the given numbers are outside range 0-255.
  - `brighter :: Colour -> Colour` that makes RGB colours 10% larger.
     None of the RGB components can be larger than 255.
  - `toHexString :: Colour -> String` converts the colour object into a hex string, e.g., `toHexString White` is `"#FFFFFF"`.
  - Advanced: `fromHexString :: String -> Either String Colour` converts a hex string into a `Colour` object.
    Returns an appropriate error message if the input string is not well formatted.
