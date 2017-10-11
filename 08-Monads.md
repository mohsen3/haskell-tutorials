# What is `Monad`?

> A monad is a monoid in the category of endofunctors

No, seriously! It's not a joke!
That's a technically correct definition, but it's not so helpful in practice.


# So, what is `Monad`?

```haskell
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b -- bind operator
```

`Monad` class also defines `>>` (sometimes called the sequence operator) with the default definition of:

```haskell
(>>) :: m a -> m b -> m b
m >> k = m >>= \_ -> k
```

# `Monad`s are `Applicative`s and `Functors`s

Let `bind = flip (>>=)`.
We can see the similarity and differences between the three.

```haskell
fmap :: Functor f     =>   (a -> b) -> f a -> f b
<*>  :: Applicative f => f (a -> b) -> f a -> f b
bind :: Monad f       => (a -> f b) -> f a -> f b
```
Monads are more powerful than Applicatives and Applicatives are more powerful than Functors.
We can define 

```haskell
fmap f xs = xs >>= return . f
```

For applicatives `pure` is the same as `return` and `<*>` can be written as

```haskell
ap m1 m2 = m1 >>= \x1 -> (m2 >>= \x2 -> return (x1 x2))
```

Additionally, `(*>)` is the same as `(>>)`.


