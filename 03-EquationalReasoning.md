
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

