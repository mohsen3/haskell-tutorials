name: inverse
layout: true
class: center, middle, inverse
---
### Introduction to Haskell
# The Basics
## Mohsen Mollanori
---
layout: false
## The quicksort example

```haskell
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (>x) xs)
```
--
```ruby
def quick_sort(array, beg_index, end_index)
  if beg_index < end_index
    pivot_index = partition(array, beg_index, end_index)
    quick_sort(array, beg_index, pivot_index -1)
    quick_sort(array, pivot_index + 1, end_index)
  end
  array
end

def partition(array, beg_index, end_index)
  current_index = beg_index
  i = beg_index
  while i < end_index do
    if array[i] <= array[end_index]
      swap(array, i, current_index)
      current_index += 1
    end
    i += 1
  end
  swap(array, end_index, current_index)
  current_index
end
```
---
## Functional style quicksort in Ruby
```haskell
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<x) xs) ++ [x] ++ quicksort (filter (>x) xs)
```

```ruby
def quicksort(*ary)
  return [] if ary.empty?

  pivot = ary.delete_at(rand(ary.size))
  left, right = ary.partition(&pivot.:>)

  *quicksort(*left), pivot, *quicksort(*right)
end
```

---
## A bit of history: Turing machine vs. lambda calculus
- Lambda calculus (1928-1929) -- Alonzo Church 
- Turing machine (1935-1937) -- Alan Turing
---

# Haskell features
 - Purely functional -> No side effect for functions, no `for/while` loop, referencial transparency
 - Immutable variables by default
 - Strongly typed with type inference
 - Lazy (non-strict) -> results are evaluated only if they are required
 - Layout based syntax (like Python, unlike C/C++)

---

# Is it more compiley or interprety?
 
- Haskell is a compiled language
- It has an interpreter and a REPL as well

---
# 🛠 stack

```bash
stack ghci # <- We start here, the interactive shell
stack ghc
stack setup
stack new
stack build
stack run
stack --help
```

---

# Defining a function

```haskell
f x = x * x
```


```haskell
g x y z = x + y + z
```

---

# No side effect *

```ruby
x = 1

def f(n)
  x = x + 1
  x + n
end
```
---

# let/in syntax

```haskell
f x = let x1 = x + 1 in x1 * x1 -- computing (x + 1) ^ 2
```

---

# let/in syntax (2)

```haskell
g x =
   let x1 = x + 1
       x2 = x + 2
   in
       x1 + x2
```

---

# The `where` keyword

```haskell
f x = x1 + x2
   where x1 = x + 1
         x2 = x + 2
```

---

# Explicit type signatures

```haskell
f :: Int -> Int
f x = x + 1
```
--

```haskell
f :: Float -> Float
f x = x + 1
```

---

# Explicit type signatures (2)

```haskell
add :: Int -> Int -> Int
add a b = a + b
```

---

# Pattern matching

In function parameters:

```haskell
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

---

# Pattern matching (2)

In function parameters:

```haskell
first  (x, y) = x
second (x, y) = y
```

```haskell
first  (x, _) = x
second (_, y) = y
```

---

# Pattern matching (3)

In function parameters:

```haskell
say :: Int -> String
say 1 = "one"
say 2 = "two"
say 3 = "three"
say 0 = "what?!"
say _ = "many"
```

---

# Pattern matching (4)

And in assignments:

```haskell
(first, second, third) = (1, 2, 3)
```

---

# ❓Planet Name

https://www.codewars.com/kata/515e188a311df01cba000003/train/haskell

---

# Guards

```haskell
fib n
   | n <= 1 = 1
   | otherwise = fib (n - 1) + fib (n - 2)
```

---

# ❓Area or Perimeter

https://www.codewars.com/kata/5ab6538b379d20ad880000ab/train/haskell

---
# Declaring new operators

```haskell
x *** y = (x + 1) * (y + 1)
```

Similarly:
```haskell
(***) x y = (x + 1) * (y + 1)
```

---

# Operators are functions and functions are operators

Using the `+` operator like a function
```haskell
three = (+) 1 2
```

Infix application of functions:
```haskell
add x y = x + y

three = 1 `add` 2
```

---

# Passing functions to other functions

```haskell
f g x = g (g (g x))
```

## 📚 Exercise

What is the type of function `f`?


---

# Returning functions from functions

```haskell
f flag = if flag then (+) else (-)
```

## 📚 Exercise

What is the type of function `f`?


---

# 📚 Exercise

Complete the body of the function `rep` so that the returned value is the result
of repeatedly applying function `f` to `x`, `n` times.

```haskell
rep :: Int -> (Int -> Int) -> Int -> Int
rep n f x = ...
```

E.g., `rep 3 f 1` is `f (f (f 1)))`.


---


# Currying

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

---

# Function composition

```haskell
import Data.List (sort)

revSort = reverse . sort
```


---

# Function composition (2)

```haskell
myOdd = not . even
```

---

# Function composition (3)

```haskell
add a b = a + b
square x = x * x
plusOneSquared = squared . add 1
```


---

# Function composition (4)

```haskell
plusOneSquared = (^2) . (1+)
```


---

# Lists

- A list can hold only one type of object, we cannot mix, e.g., `Int`s and `String`s
--
- `[]` is the empty list constructor
--
- `:` is an operator (known as list constructor) that allows you to prepend an element to a (possibly empty) list
--
- To append two lists, use the `++` operator
--
- To append a single element `a` to list `l`, use `l ++ [a]`


---

# List Examples

```haskell
[]                      -- empty list
[1,2,3]                 -- List of integral
["foo","bar","baz"]     -- List of String
[1,2,"foo"]             -- ERROR! Cannot mix types in a list
[(1, "aa"), (2, "bb")]  -- List of tuples
1:[2,3]                 -- [1,2,3], (:) prepend one element
1:2:[]                  -- [1,2]
[1,2] ++ [3,4]          -- [1,2,3,4], (++) concatenate
[1,2,3] ++ ["foo"]      -- ERROR String ≠ Integral
[1..4]                  -- [1,2,3,4]
[1,3..10]               -- [1,3,5,7,9]
[2,3,5,7,11..100]       -- ERROR! I am not so smart!
[10,9..1]               -- [10,9,8,7,6,5,4,3,2,1]
```

---

# Pattern matching on lists

```haskell
head :: [a] -> a
head (h:r) = h

tail :: [a] -> [a]
tail (h:r) = r
```

---

# Pattern matching on lists (2)

```haskell
head' (h:_) = h
tail' (_:r) = r
```

---

# Pattern matching on lists (3)


```haskell
threeIsOk [_, _, _] = True
threeIsOk _ = False
```

---

# ❓ Fake Binary

https://www.codewars.com/kata/57eae65a4321032ce000002d/train/haskell

---

# List comprehension

```haskell
squares = [ x * x | x <- [1..5] ] -- [1,4,9,16,25]
```
---

# List comprehension -- With two iterators
```haskell
[ (i, j) | i <- [1, 3], j <- [4..7] ] 
  -- Output:
  -- [(1,4),(1,5),(1,6),(1,7),(3,4),(3,5),(3,6),(3,7)]
```
---

# List comprehension -- With filters
```haskell
[ x * x | x <- [1..5], odd x ] -- [1,9,25]
```

---

# ❓ Count by X

https://www.codewars.com/kata/5513795bd3fafb56c200049e/train/haskell

---
# Infinite lists

```haskell
ones = 1 : ones
zeros = 0 : zeros
numsFrom n = n : numsFrom (n + 1)
```

---

# ❓ Find odd in the list

https://www.codewars.com/kata/54da5a58ea159efa38000836/train/haskell

#### Tips:
```haskell
import Data.List (group, sort)

head :: [a] -> a
filter :: (a -> Bool) -> [a] -> [a]
odd :: Integral a => a -> Bool
group :: Eq a => [a] -> [[a]]
sort :: Ord a => [a] -> [a]
length :: [a] -> Int -- Almost!
```
