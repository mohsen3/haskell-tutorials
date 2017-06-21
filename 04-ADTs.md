
# Algebraic data types

The simplest form

```haskell

data Bool = True | False

data DayOfWeek =  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

```

Constructors can have fields

```haskell
data Person = Person String Int String

data Tree = Internal Tree Int Tree | Leaf Int

data LinkedList = Const Int LinkedList | Nil
```

Data types can be parameterized

```haskell
data Tree a = Internal (Tree a) a (Tree a) | Leaf a

data LinkedList a = Cons a (LinkedList a) | Nil
```
