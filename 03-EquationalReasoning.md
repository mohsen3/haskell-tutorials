
## Anonymous functions

```haskell
\x -> x + 1
```

```haskell
\x y -> x + y
```

```haskell
newList = zipWith (\x y -> x ^ 2 + y) [1, 2, 3] [4, 5, 6]
```

## Eta reduction

```haskell
f x y z = g (h x y) z
```

```haskell
add x y = x + y
add x y = (+) x y
add = (+)
```

## Point free style of functions

```haskell
f :: (a, [a]) -> [a] -> [a]
f (c, w1) w2 = c:w1 ++ w2

-- Prefix style of ++
f (c, w1) w2 = (++) (c:w1) w2

-- Eta reduction
f (c, w1) = (++) (c:w1)

-- Prefix style of :
f (c, w1) = (++) ((:) c w1)

-- Use uncurry
f x = (++) (uncurry (:) x)

-- f x = g (h x) where g = (++); h = uncurry (:)
-- Composition
f = g . h where g = (++)
                h = uncurry (:)
                

-- Plug back g and h
f = (++) . uncurry (:)
```

