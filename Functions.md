# Functions by Example

### How to make a function pure

Dependency on global state `g`

```python
g = 1

def f(n):
  g = g + 1
  return n + g
  
# ...
y = f(3) + 4

```

can typically be removed by making the global variable a parameter of the function:

```python
g = 1

def f(g, n):
  g1 = g + 1
  return (g1, n + g1)
  
# ---
(g2, y) = f(g, 3) + 4

```

### Expressions
Everything in Haskell is either an expression or decration.
Expressions consist of
- Literals, e.g, `123`, `"ABC"`, ...
- Variables
- Functions
- Operators

### Defining functions

```haskell
f x = x + 1
```

```haskell
g x y z = x + y + z
```

### `let/in` keywords

```haskell
f x = let x1 = x + 1 in x1 * x1 -- computing (x + 1) ^ 2
```

```haskell
g x =
   let x1 = x + 1
       x2 = x + 2
   in
       x1 + x2
```

### `where` keyword

```haskell
f x = x1 + x2
   where x1 = x + 1
         x2 = x + 2
```

### Explicit type signatures

```haskell
f :: Int -> Int
f x = x + 1
```

```haskell
f :: Float -> Float
f x = x + 1
```

### Declaring new operators

```haskell
x *** y = (x + 1) * (y + 1)
```

Similarly:
```haskell
(***) x y = (x + 1) * (y + 1)
```

### Operators are functions and functions are operators

Using the `+` operator like a function
```haskell
three = (+) 1 2
```

Infix application of functions:
```haskell
add x y = x + y

three = 1 `add` 2
```

### Currying

```haskell
f :: Int -> Int -> Int
f x y = x + y
```

```haskell
> :type f 1 
f 1 :: Int -> Int
```



