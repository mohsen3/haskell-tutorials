name: inverse
layout: true
class: center, middle, inverse
---
### Introduction to Haskell
# Data Types
### Mohsen Mollanori
---
layout: false
# Algebraic data types

---

## The simple form
```haskell
data DayOfWeek =  
   Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
```

---

## The boolean datatype

```haskell
data Bool = True | False

```
--
```haskell
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False
```
--
```haskell
(||) :: Bool -> Bool -> Bool
False || False = False
_     || _     = True
```

---

## Constructors can have fields

```haskell
data Person = Person String Int String
```
--

```haskell
data Colour = Black | White | Gray |
              RGB Int Int Int |
              RGBA Int Int Int Int
```

---
## Pattern Matching on Constructors

```haskell
data Shape = Circle Double |
             Rectangle Double Double |
             Square Double |
             Triangle Double Double

```

--

```haskell
area :: Shape -> Double
area (Circle r) = 3.14 * r * r
area (Rectangle a b) = a * b
area (Square a) = a * a
area (Triangle h r) = 0.5 * r * h
```

---
## Data Types can be Recursive

```haskell
data Tree = Internal Tree Int Tree | Leaf Int
```

--

```haskell
data LinkedList = Cons Int LinkedList | Nil
```
---

## Data types can be parameterized

```haskell
data Tree a = Internal (Tree a) a (Tree a) | Leaf a
```
---

## Pattern matching on ADTs

```haskell
data LinkedList a = Cons a (LinkedList a) | Nil
```

--

```haskell
size :: LinkedList a ->  Int
size Nil = 0
size (LinkedList r) = 1 + size r
```

---

## Constructors are (similar to) functions

```haskell
λ> data LinkedList = Cons Int LinkedList | Nil

λ> :t Cons
Cons :: Int -> LinkedList -> LinkedList

λ> :t Nil
Nil :: LinkedList
```

---

## The `deriving` keyword

```haskell
data DayOfWeek =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
                      deriving (Eq, Ord, Show, Read, Enum, Bounded)
```

---

## `Maybe` Data Type

```haskell
data Maybe a = Nothing | Just a
```

--

```haskell
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
```

---
## `Either` Data Type

```haskell
data Either a b = Left a | Right b
```

--

```haskell
data Date = Date {year :: Int, month :: Int, day :: Int} deriving (Eq, Show)
mkDate :: Int -> Int -> Int -> Either String Date
mkDate y m d
  | d > 31 || d < 1 = Left ("Incorrect day " ++ show d)
  | m > 12 || m < 1 = Left ("Unknown month " ++ show m)
  | otherwise       = Right $ Date y m d
```

---

## Data constructors can be operators

```haskell
data List a = Empty | a ::: List a deriving (Eq, Ord, Show, Read)
infixr 1 :::

list = 1 ::: 2 ::: 3 ::: Empty
```

---

# 📚 Exercise

Consider the following definition of `LinkedList`:

```haskell
data LinkedList a = Cons a (LinkedList a) | Nil
```

Implement the following functions:

```haskell
size :: LinkedList a -> Int
size ll = undefined

get :: LinkedList a -> Int -> Maybe a
get ll i = undefined

prepend :: LinkedList a -> a -> LinkedList a
prepend ll x = undefined 

```
