# What is `Monad`?

> A monad is a monoid in the category of endofunctors

No, seriously! It's not a joke!
That's a technically correct definition, but it's not so helpful in practice.


# So, what is `Monad`?

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b -- bind operator
  return :: a -> m a
```

`Monad` class also defines `>>` operator with the default definition of:

```haskell
(>>) :: m a -> m b -> m b
m >> k = m >>= \_ -> k
```


