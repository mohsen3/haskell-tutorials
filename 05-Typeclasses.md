
## Typeclasses

```haskell
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> Maybe a
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

## A complete example

```haskell
module Tutorials.Tree (Tree, empty, insert, contains) where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Ord)

instance Show a => Show (Tree a) where
  show Nil = ""
  show (Node l x r) = concat [ show x, "(", show l, ",", show r, ")"]

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
