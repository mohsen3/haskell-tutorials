
# `Monoid`

Monoid is typeclass of things that
  1. Can be **combined** to make a thing of the same type
  2. There is an identity or **null** object so that combining it with anything has no effect


```haskell
class Monoid a where
  {-# MINIMAL mempty, mappend #-}
  mempty :: a
  
  mappend :: a -> a -> a
  
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
```

```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

```haskell
data GPA = GPA {getGPA :: Double, numUnits :: Int} deriving (Eq, Show)

instance Monoid GPA where
  mappend (GPA g1 u1) (GPA g2 u2) = GPA ((ud1 * g1 + ud2 * g2) / (ud1 + ud2)) (u1 + u2)
    where
      ud1 = fromIntegral u1
      ud2 = fromIntegral u2
      
  mempty = GPA 0.0 1
```

### Monoid laws

```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

### The `<>` operator

```haskell
-- infix synonym for mappend, defined in Data.Monoid
x <> y = mappend x y
infixr 6 <>

GPA 4.0 6 <> GPA 3.0 3
```

### The `newtype` trick

Sometimes,
there is more than one instance of a typeclass for a datatype.
For instance,
there are at least two possible ways to concat numbers,
using `+` and `*` operators.
In such cases,
we can make a `newtype` wrapper for each instance.

```haskell
newtype Sum n = Sum n
 
instance Num n => Monoid (Sum n) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)


newtype Product n = Product n
 
instance Num n => Monoid (Product n) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)
```


## `Functor`

Functor is the typeclass of things that can apply a function to the inner component(s).

```haskell
class Functor f where
  {-# MINIMAL fmap #-}
  fmap :: (a -> b) -> f a -> f b
 
  (<$) :: a        -> f b -> f a
  (<$) = fmap . const
```

For lists,
`fmap` is the same as `map`.

```haskell
λ> fmap succ [3, 4, 2]
[4, 5, 3]
```

Here is the instance declaration for `Maybe`:

```haskell
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing  = Nothing
  fmap g (Just a) = Just (g a)
```

```haskell
λ> fmap succ (Just 3)
Just 4

λ> fmap succ Nothing
Nothing

λ> fmap (fmap succ) [Just 3, Nothing, Just 4]
[Just 4,Nothing,Just 5]

```

### Functor laws
`fmap` does not change the structure of the container,
just the elements.

```haskell
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
```

### The `<$>` operator

```haskell
infixl 4 <$>
(<$>) = fmap
```

```haskell
λ> succ <$> Just 3
Just 4
```

## `Foldable`

```haskell
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    -- | Map each element of the structure to a monoid,
    -- and combine the results.
    foldMap :: Monoid m => (a -> m) -> t a -> m
    -- This INLINE allows more list functions to fuse. See Trac #9848.
    foldMap f = foldr (mappend . f) mempty

    -- | Right-associative fold of a structure.
    foldr :: (a -> b -> b) -> b -> t a -> b
    
    -- several other functions including
    -- fold, foldl,
    -- toList, null, length, elem, 
    -- maximum, minimum, sum, product
```

Module `Data.Foldable` also adds the following functions (and a few more) to any `Foldable`:

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


We can define a `Foldable` instance for `Tree` as follows:

```haskell
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord)

instance Foldable Tree where
  foldr _ z Nil = z
  foldr f z (Node l d r) = foldr f (f d (foldr f z l)) r
```

Interestingly,
GHC can automatically derive `Foldable` and `Functor` instances for us
using the `DeriveFoldable` and `DeriveFunctor` extensions:
```haskell
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show, Eq, Ord, Functor, Foldable)
```

## `Applicative`

Applicatives allow you to put a function into a *contest* and apply it onto an object which resides inside the same context.

```haskell
class Functor f => Applicative f where
    {-# MINIMAL pure, ((<*>) | liftA2) #-}

    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    -- | Lift a binary function to actions.
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- | Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
    
    -- | Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const
```

Helper functions:

```haskell
-- | Lift a function to actions.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c
```

Applicative instances:

```haskell
instance Applicative (Either e) -- Defined in ‘Data.Either’
instance Applicative [] -- Defined in ‘GHC.Base’
instance Applicative Maybe -- Defined in ‘GHC.Base’
instance Applicative ((->) a) -- Defined in ‘GHC.Base’
instance Monoid a => Applicative ((,) a) -- Defined in ‘GHC.Base’
```


# `Alternative`

```haskell
class Applicative f => Alternative f where
    {-# MINIMAL empty, (<|>) #-}
    -- | The identity of '<|>'
    empty :: f a

    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]

    -- | Zero or more.
    many :: f a -> f [a]
```

```haskell
instance Alternative [] -- Defined in ‘GHC.Base’
instance Alternative Maybe -- Defined in ‘GHC.Base’
```


## :ledger: Homework
Write a `Monoid` instance for `Tree a`.

## :ledger: Homework
The main advantage of the *binary search tree* (BST) data structure is that operations like `elem` and `find` can be done in time `O(log n)` (on average).

  1. Explain why the above definition of `Foldable` instance for `Tree` results in an `O(n)` algorithm for `elem`.
  2. Write a custom `treeElem` function that works in `O(log n)` rather than `O(n)`.

## :ledger: Homework
Update the definition of the `Tree` datatype to be:
```haskell
data Tree a = Nil | 
              Node { left     :: Tree a
                   , nodeData :: a
                   , right    :: Tree a
                   , size     :: Int
                   } deriving (Show, Eq, Ord)
```
The new `size` field to keeps the size of the sub-tree.
Update `insert` and `length` so that they work with the new `size` field.
Note that `length` works in `O(1)` now.

## :ledger: Homework
`Semigroup` is a typeclass that has a `<>` operator (similar to `Monoid` `<>` operator) but no `mempty`.
Semigroup is the class of things that can be concatinated,
but do not have any null instance.
The class is placed in `Data.Semigroup` module that is available from the `semigroup` package.
The `semigroup` package also provides `Data.List.NonEmpty` that is a non-empty list of items with the following definition:

```haskell
data NonEmpty a = a :| [a]
```
No, `:|` is not a smiley.
It's the constructor of the `NonEmpty` type.
`NonEmpty` is a `Semigroup`.
The package also provides `newtype` wrappers for `Min a` and `Max a` (similar to `Sum a` and `Product a`).
Explain why `Max Int` is a monoid,
while `Max Integer` is a semigroup.

