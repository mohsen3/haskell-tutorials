# nano-lang

This is a very basic parsing project that I developed during the tutorial sessions.
The parser consists of [a simple parsing library](src/ParserLib/SimpleParser.hs) that is used to define the [_nano-lang_ parser](src/Nano/Parser.hs). _nano_ is an extremely simple programming language with the following syntax:

```
def f(x,y)=x+2*y
let y=f(3,4)
let z=y+3*f(y,3)
print z
```

There is also a partially implemented [`Evaluator`](src/Nano/Evaluator.hs) for _nano_ with lots of `undefined` expressions.
Completing the evaluator is left to the reader as a homework.
