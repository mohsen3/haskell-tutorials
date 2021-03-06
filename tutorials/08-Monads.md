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
In the following `con` denotes the `Functor`, `Applicative` or `Monad` context.

```haskell
fmap :: Functor con     =>     (a -> b) -> con a -> con b
<*>  :: Applicative con => con (a -> b) -> con a -> con b
bind :: Monad con       => (a -> con b) -> con a -> con b
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

## The `do` notation

Haskell has a handy syntactic sugar for Monads

```haskell
main = do
   putStr "What's your name? "
   name <- getLine
   putStrLn ("hello " ++ name)
```

It makes Haskell look like imparative programming, but it's not.
It's just a syntax sugar.

## An example Monad

```haskell

data Logged a = Logged [String] a

instance Functor Logged where
  fmap f (Logged msgs x) = Logged msgs (f x)

instance Applicative Logged where
  pure x = Logged [] x
  Logged msgs1 f <*> Logged msgs2 x = Logged (msgs1 ++ msgs2) (f x)

instance Monad Logged where
  return = pure
  Logged msgs x >>= f = let Logged msgs2 x2 = f x in Logged (msgs ++ msgs2) x2

logMsg :: String -> Logged ()
logMsg msg = Logged [msg] ()

get1 :: Logged Int
get1 = do
  logMsg "returning 1"
  return 1

runLogged :: Logged a -> ([String], a)
runLogged (Logged msg x) = (msg, x)

main = do
  let (msgs, res) = runLogged $ do
    one <- get1
    logMsg $ "got " ++ show one
    logMsg "Finished"
    return one

  print msgs
  print res

```


## Further Reading

- [The State Monad: A Tutorial for the Confused?](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
- [Haskell: Generating random numbers](http://www.markhneedham.com/blog/2012/05/08/haskell-generating-random-numbers/)
- [Monad Examples](https://github.com/phischu/monad-examples)
