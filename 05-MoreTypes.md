
## Return type polymorphism

```haskell
λ> n = 1
λ> :type n
n :: Num t => t

λ> i = n :: Int
λ> :type i
i :: Int

λ> d = n :: Double
λ> :type d
d :: Double
```

## Type synonyms

```haskell
type Name = String
type Age = Int

data Person = Person { firstName :: Name, lastName :: Name, age :: Age }

```

## `newtype` keyword

Loosely speaking, `newtype` is like `data` with one constructor and one field.

```haskell
newtype Age = Age Int deriving (Eq, Ord, Show)

isOld :: Age -> Bool
isOld (Age age) = if age > 80 then True else False
```

## Typeclasses

```haskell
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> (Maybe a, [Int]) -- if possible decode the object, return the remainder as well
```

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  x == y = not (x /= y)
  
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)
  
  {-# MINIMAL (==) | (/=) #-}
```

```haskell
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  
  x - y = x + negate y
  negate x = 0 - x

  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
```

## Instance declaration

```haskell

data Binary = Zero | One deriving (Eq, Show, Read, Ord, Enum, Bounded)

instance Num Binary where
    Zero + x = x
    One  + x = One

    Zero * x = Zero
    One  * x = x

    negate = id
    abs = id
    signum = id
    
    fromInteger 0 = Zero
    fromInteger _ = One

```

```haskell

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Char

instance Serializable Int where
  serialize n = [n]
  
  deserialize (n:r) = (Just n, r)
  deserialize empty = (Nothing, empty)
  
  
instance Serializable String where
  serialize str = length str:map ord str
  
  deserialize [] = (Nothing, [])
  deserialize (l:r) =
    if length r >= l
    then (Just $ map chr $ take l r, drop l r)
    else (Nothing, r)

```

## A complete example

```haskell
module Tutorials.Tree (Tree, empty, insert, contains) where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show, Read)

empty = Nil

insert x Nil = Node Nil x Nil
insert x (Node left d right) =
  if x > d
    then Node left d (insert x right)
    else Node (insert x left) d right

contains x Nil = False
contains x (Node l e r)
  | x == e = True
  | x < e = contains x l
  | otherwise = contains x r

```

### Implementing the instances manually (not using `deriving`)
```haskell
data Tree a = Nil | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
  show Nil = ""
  show (Node l x r) = concat [ show x, "(", show l, ",", show r, ")"]

instance Eq a => Eq (Tree a) where
  Nil == Nil = True
  Node r1 x l1 == Node r2 y l2 =
    x  == y  &&
    r1 == r2 && 
    l1 == l2
  _ == _ = False

instance Ord a => Ord (Tree a) where
  Nil <= _ = True
  Node _ _ _ <= Nil = False
  Node l1 x r1 <= Node l2 y r2 =
    (l1 < l2) ||
    (l1 == l2 && x < y) ||
    (l1 == l2 && x == y && r1 <= r2)
```
