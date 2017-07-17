
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

## `Applicative`

## `Foldable`

