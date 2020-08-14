name: inverse
layout: true
class: center, middle, inverse
---
### Introduction to Haskell
# Typeclasses
### Mohsen Mollanori
---
layout: false

# Typeclasses

```haskell
class Serializable a where
  serialize :: a -> [Byte]
  
  deserialize :: [Byte] -> (Maybe a, [Byte]) 
    -- if possible decode the object
    -- return the remainder bytes as well
```

---

# Eq

```haskell
class Eq a where
  (==) :: a -> a -> Bool

  (/=) :: a -> a -> Bool

  {-# MINIMAL (==) | (/=) #-}
  	-- Defined in ‘GHC.Classes’
```
---


# Ord

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
  	-- Defined in ‘GHC.Classes’
```
--

```haskell
data Ordering = LT | EQ | GT 	-- Defined in ‘GHC.Types’
```

---

# Num

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

---
## Instance declaration


```haskell
data Binary = Zero | One deriving (Eq, Show, Read, Ord, Enum, Bounded)
```

--

```haskell
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

---

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

---
## Implementing the instances manually 
without using `deriving` keyword

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
```

---

## Monoid

```haskell
class Monoid a where
  {-# MINIMAL mempty, mappend #-}
  mempty :: a
  
  mappend :: a -> a -> a
  
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
```


---
## Monoid Example

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

---
## Another example

```haskell
data GPA = GPA {getGPA :: Double, numUnits :: Int} deriving (Eq, Show)
```

--

```haskell
instance Monoid GPA where
  mappend (GPA g1 u1) (GPA g2 u2) = GPA ((ud1 * g1 + ud2 * g2) / (ud1 + ud2)) (u1 + u2)
    where
      ud1 = fromIntegral u1
      ud2 = fromIntegral u2
      
  mempty = GPA 0.0 1
```

---

## The `<>` operator

```haskell
-- infix synonym for mappend, defined in Data.Monoid
x <> y = mappend x y
infixr 6 <>

GPA 4.0 6 <> GPA 3.0 3
```

---

## Functor

```haskell
class Functor f where
  {-# MINIMAL fmap #-}
  fmap :: (a -> b) -> f a -> f b
 
  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```


---

## An example

```haskell
λ> fmap succ [3, 4, 2]
[4, 5, 3]
```

---

## Another example (Maybe functor instance)

```haskell
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap g (Just a) = Just (g a)
```

--

```haskell
λ> fmap succ (Just 3)
Just 4

λ> fmap succ Nothing
Nothing

λ> fmap (fmap succ) [Just 3, Nothing, Just 4]
[Just 4,Nothing,Just 5]
```

---

## The `<$>` operator

```haskell
infixl 4 <$>
(<$>) = fmap
```

--

```haskell
λ> succ <$> Just 3
Just 4
```

---

## Foldable

```haskell
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
    foldMap f = foldr (mappend . f) mempty

    -- | Right-associative fold of a structure.
    foldr :: (a -> b -> b) -> b -> t a -> b
    
    -- several other functions including
    -- fold, foldl,
    -- toList, null, length, elem, 
    -- maximum, minimum, sum, product
```


---

## `Data.Foldable` defines the following functions as well

```haskell
concat :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
and :: Foldable t => t Bool -> Bool
or :: Foldable t => t Bool -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool
all :: Foldable t => (a -> Bool) -> t a -> Bool
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
```

---

## Applicative


```haskell
class Functor f => Applicative (f :: * -> *) where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  {-# MINIMAL pure, ((<*>) | liftA2) #-}
  	-- Defined in ‘GHC.Base’
```

---

## Alternative

```haskell
class Applicative f => Alternative (f :: * -> *) where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  {-# MINIMAL empty, (<|>) #-}
  	-- Defined in ‘GHC.Base’
```
