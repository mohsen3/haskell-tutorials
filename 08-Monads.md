## What is `Monad`?

> A monad is a monoid in the category of endofunctors

No, seriously! It's not a joke!
That's a technically correct definition, but it's not so helpful in practice.


## So, what is `Monad`?

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

## `Monad` is more powerful than `Applicative` and `Functor`

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
m1 <*> m2 = m1 >>= \x1 -> (m2 >>= \x2 -> return (x1 x2))
```

Additionally, `(*>)` is the same as `(>>)`.


## Where does this power come from?

Given a list, say we want to duplicate each item in the list.
E.g., given `[1,2,3]` we want to produce `[1,1,2,2,3,3]`.
We define function

```haskell
dup x = [x, x]
```

Then we can use `fmap` to duplicate all the items in the given list:

```haskell
dupList xs = fmap dup xs
```

But the result of `dupList [1,2,3]` is `[[1,1],[2,2],[3,3]]`.
In fact, `Functor`s cannot do any better.
Applicatives are the same.
Once we introduce a new layer of structure (here the list structure),
we cannot remove it using the mechanism that `Functor` and `Applicative` suggest.
But `Monad` can do that.

```haskell
λ> [1,2,3] >>= dup
[1,1,2,2,3,3]
```

`Monad` is sometimes defined in terms of `return` and `join` where

```haskell
join :: Monad m => m (m a) -> ma
join x = x >>= id
```
Note that if we have `return` and `join`, we can write bind as

```haskell
ma >>= f = join (fmap f ma)
```

## `IO` is a `Monad`

One of the amazing things about Monads is that they can simulate side effects.

```haskell
main :: IO ()
main = putStr "What's your name? " >> getLine >>= \name -> putStrLn ("hello " ++ name)
```


