# Introduction

### A bit of history
- Turing machine vs. lambda calculus
- Imperative languages vs. functional languages
- Typed lambda calculus
- Lisp, ML, Miranda, ...

### Haskell features
 - Purely functional -> No side effect for functions, no `for/while` loop, referencial transparency
 - Immutable variables by default
 - Strongly typed with type inference
 - Lazy (non-strict) -> results are evaluated only if they are required
 - Layout based syntax (like Python, unlike C/C++)

### Is it more compiley or interprety?
 
Haskell is a compiled language.
It has an interpreter and a REPL as well,
that help you throughout the development process.
However,
it is known for its strong compile time checks and guarantees.
Actually,
Haskell has one of the most sophisticated compilers that I have seen among all the languages.
One of the cool features of Haskell is that once your program is compiled,
it works at least 9 out of 10 times.
It's unlike e.g.,
Python or JavaScript that most of the errors are caught during run-time.
Errors like
"null pointer exception",
"array index out of bound",
and "class cast exception",
that are common in imperative languages,
rarely or never happen in Haskell.
The compiler and the way language is designed helps you write large programs with complicated logic better than most other languages.

### Where is it most commonly used these days?

Haskell is a general purpose programming language (like python, C++, Java, etc).
It can be used for pretty much anything you can program.
It is commonly used for backend development,
but it is getting popular on the frontend as well (you can even compile it into Javascript).
Here is a list of few companies that use Haskell:
https://wiki.haskell.org/Haskell_in_industry

# Tools

The Haskell standard has [many different implementations](https://wiki.haskell.org/Implementations),
among which GHC (Glasgow Haskell Compiler) is the most popular.
GHC supports Haskell 2010 standard and many non-standard *extensions* to the language.
GHC toolset contains the following tools:
 - `ghc`: Haskell compiler, think of it as `gcc` for Haskell
 - `ghci` interactive shell or REPL for Haskell
 - `runhaskell`: the interpreter for Haskell,
 - `hpc`: code coverage
 - `haddock`: document generator from source code
 
 ### Stack
 
 `stack` is a build tool and more for Haskell.
 It allows you to build your projects,
 install the required packages,
 and even setup the required tools for you.
 The easiest way to install GHC is to use `stack` to do so.
 First install `stack` for your platform
 (See [this link](https://docs.haskellstack.org/en/stable/install_and_upgrade/) for the instructions).
 Typically, you can download `stack` as a single pre-compiled statically-linked binary and put it in your `$PATH`.
 Once stack is installed, run `stack setup` to download and install GHC and a set of basic libraries.
 Be patient!
 It may take a long time (more than half an hour, if I remember correctly),
 but eventually it installs everything you need to start with programming Haskell.
 `stack` installs a sandbox copy of GHC and libraries inside your home folder,
 by default.
 To run `ghci`, you need to execute `stack ghci` then.
 
 
 
