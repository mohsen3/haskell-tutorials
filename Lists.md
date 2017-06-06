
## Lists

The following snippet is stolen from [here](http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/#lists).

```haskell
[]                      ⇔ empty list
[1,2,3]                 ⇔ List of integral
["foo","bar","baz"]     ⇔ List of String
[1,2,"foo"]             ⇔ ERROR! Cannot mix types in a list
1:[2,3]                 ⇔ [1,2,3], (:) prepend one element
1:2:[]                  ⇔ [1,2]
[1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concatenate
[1,2,3] ++ ["foo"]      ⇔ ERROR String ≠ Integral
[1..4]                  ⇔ [1,2,3,4]
[1,3..10]               ⇔ [1,3,5,7,9]
[2,3,5,7,11..100]       ⇔ ERROR! I am not so smart!
[10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
```

- A list can hold only one type of object, we cannot mix, e.g., `Int`s and `String`s
- `:` is an operator (known as list constructor) that allows you to prepend an element to a (possibly empty) list
- To append two lists, use the `++` operator
- To append a single element `a` to list `l`, use `l ++ [a]`



### Infinite lists

```haskell
ones = 1 : ones
zeros = 0 : zeros
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
