

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
