
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

## `Applicative`

## `Foldable`

## :ledger: Homework
`Semigroup` is a typeclass that has a `<>` operator (similar to `Monoid` `<>` operator) but no `mempty`.
Semigroup is the class of things that can be concatinated,
but do not have any empty instance.
The class is placed in `Data.Semigroup` module that is available from the `semigroup` package.
The `semigroup` package also provides `Data.List.NonEmpty` that is a non-empty list of items with the following definition:

```haskell
data NonEmpty a = a :| [a]
```
`NonEmpty` is a `Semigroup`.
The package also provides `newtype` wrappers for `Min a` and `Max a`.
Explain why `Max Int` is a monoid,
while `Max Integer` is a semigroup.

