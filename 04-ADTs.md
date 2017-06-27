
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
              CMYK Int Int Int Int

data Shape = Circle Double |
             Rectangle Double Double |
             Square Double |
             Triangle Double Double

```

Constructors are similar to functions

```haskell
λ> data LinkedList = Cons Int LinkedList | Nil
λ> :t Cons
Cons :: Int -> LinkedList -> LinkedList
λ> :t Nil
Nil :: LinkedList
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
data DayOfWeek =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Enum, Bounded)
```

## Data constructors can be operators

```haskell
data List a = Empty | a ::: List a deriving (Eq, Ord, Show, Read)
infixr 1 :::

list = 1 ::: 2 ::: 3 ::: Empty
```

