
## Introduction

The following snippet is stolen from [here](http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/#lists).

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

- A list can hold only one type of object, we cannot mix, e.g., `Int`s and `String`s
- `[]` is the empty list constructor
- `:` is an operator (known as list constructor) that allows you to prepend an element to a (possibly empty) list
- To append two lists, use the `++` operator
- To append a single element `a` to list `l`, use `l ++ [a]`


![lists in haskell are linked lists](http://s3.amazonaws.com/lyah/listmonster.png)


## Pattern matching on lists

```haskell
head :: [a] -> a
head (h:r) = h

tail :: [a] -> [a]
tail (h:r) = r
```

```haskell
head' (h:_) = h
tail' (_:r) = r
```

```haskell
threeIsOk [_, _, _] = True
threeIsOn _ = False
```

## List functions

```haskell
import Prelude hiding (length, last, reverse, take, drop, concat, map, and, zip, zipWith)

length []    = 0
length (_:r) = 1 + length r


last [x]   = x
last (_:r) = last r


reverse []    = []
reverse (h:r) = reverse r ++ [h]


take :: Int -> [a] -> [a]
take n (h:r)
   | n <= 0    = []
   | otherwise = h : take (n - 1) r


drop _ []    = []
drop n (h:r) = if n <= 0 then h:r else drop (n - 1) r


concat :: [[a]] -> [a]
concat []    = []
concat (h:r) = h ++ concat r


map :: (a -> b) -> [a] -> [b]
map _ []    = []
map f (h:r) = f h : map f r


and :: [Bool] -> Bool
and []    = True
and (h:r) = h && and r


zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _  []         = []
zip (x:r1) (y:r2) = (x, y):zip r1 r2


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []

```

`zip` can be implemented usign `zipWith`: `zip = zipWith (,)`.

### :ledger: Exercise

Implement the following functions:

- `null :: [a] -> Bool` return `True` if the given list is empty, `False` otherwise
- `elem :: a -> [a] -> Bool` return `True` if the given element is in list, `False` otherwise
  - Suggest an alternative implementation for `elem`
- `sum :: [Int] -> Int` summation of the elements of a list of integers
- `filter :: (a -> Bool) -> [a] -> [a]` keep the elements that match the criterion, drop the rest
- `splitAt :: Int -> [a] -> ([a], [a])` split the list into two smaller lists at the given index
  - Hint: use `take` and `drop`
- `all :: (a -> Bool) -> [a] -> Bool` return `True` if all the elements of the list match the given criterion, `False` otherwise
  - Hint: use `map` and `and`



## Infinite lists

```haskell
ones = 1 : ones
zeros = 0 : zeros
numsFrom n = n : numsFrom (n + 1)
```

```haskell
cyclic = let x = 0 : y
             y = 1 : x
         in  x
```

Fibonacci series
```haskell
fibs = 1:1:zipWith (+) (tail fibs) fibs
```
