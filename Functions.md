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
Everything in Haskell is either an expression or declaration.
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

- function/variable names always start with lowercase letters
- `myAwesomeFuntion`, `another_function`, and `foo'` are valid function names.
  - yes, `'` is a valid function character!

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

`let/in` can be used in any expression or sub-expression.
```haskell
3 ^ (let x = 10 in x + 1) * (let x = 3 in x * x)
```
Note the scope of the variable `x`.

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

```haskell
add :: Int -> Int -> Int
add a b = a + b
```

### Pattern matching

In function parameters:

```haskell
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

```haskell
first  (x, y) = x
second (x, y) = y
```

```haskell
first  (x, _) = x
second (_, y) = y
```

```haskell
say :: Int -> String
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 0 = "what?!"
say _ = "many"
```

And in assignments:

```haskell
(first, second, third) = (1, 2, 3)
```

### Guards

```haskell
fib n
   | n <= 1 = 1
   | otherwise = fib (n - 1) + fib (n - 2)
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

### Passing functions to other functions

```haskell
f g x = g (g (g x))
```

### :ledger: Exercise

What is the type of function `f`?


### Returning functions from functions

```haskell
f flag = if flag then (+) else (-)
```

### :ledger: Exercise

What is the type of function `f`?


## :ledger: Exercise

Complete the body of the function `rep` so that the returned value is the result
of repeatedly applying function `f` to `x`, `n` times.

```haskell
rep :: Int -> (Int -> Int) -> Int -> Int
rep n f x = ...
```

E.g., `rep 3 f 1` is `f (f (f 1)))`.

### Currying

All the functions take exactly one argument!
`f x y z` can be seen as `((f x) y) z`,
i.e,
`f x` returns a functions that takes `y` and returns a function that takes `z`.


```haskell
f :: Int -> Int -> Int
f x y = x + y
```

```haskell
> :type f 1 
f 1 :: Int -> Int
```

### Function composition

Do you remember the [f∘g](https://www.wikiwand.com/en/Function_composition) notation from high school?

```haskell
import Data.List (sort)

revSort = reverse . sort
```


```haskell
myOdd = not . even
```

```haskell
add a b = a + b
square x = x * x
plusOneSquared = squared . add 1
```

```haskell
plusOneSquared = (^2) . (1+)
```

### Fixity of operators

Each operator in Haskell has a precedence in the range `0..9`
and an associativity (which can be right, left, or none).

Here is a GHCi session.
`λ>` denotes the command entered.
The rest is the output.

```haskell
λ> :info (+)
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in `GHC.Num'
infixl 6 +

λ> :info (*)
class Num a where
  ...
  (*) :: a -> a -> a
  ...
        -- Defined in `GHC.Num'
infixl 7 *

λ> :info (^)
(^) :: (Num a, Integral b) => a -> b -> a       -- Defined in `GHC.Real'
infixr 8 ^

λ> :info (**)
class Fractional a => Floating a where
  ...
  (**) :: a -> a -> a
  ...
        -- Defined in `GHC.Float'
infixr 8 **

λ> :info mod
class (Real a, Enum a) => Integral a where
  ...
  mod :: a -> a -> a
  ...
        -- Defined in `GHC.Real'
infixl 7 `mod`
```

We can define the fixity of operators when we define them.
```haskell
x *** y = (x + 1) * y
infixl 7 ***
```

Functions have the highest precedence (10),
i.e.,
when mixed with operators,
function calls will be first.
E.g.,
`abs x + y` is the same as `(abs x) + y`.




