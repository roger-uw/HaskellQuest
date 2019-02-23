One way of defining functions in Haskell is directly describing the underlying computations with a special kind of **expressions** called lambda abstractions.

Lambda abstractions have the following form:

`(\parameters -> definition)`

_The parentheses are not mandatory, but wrapping a lambda abstraction with a pair of parentheses is suggested_

Every lambda abstraction starts with a `\` symbol to be distinguished from other expressions. The parameters are a sequence of unknown variables to be passed in as function arguments (the inputs), and this sequence ends with a `->` symbol indicating the start of function definition. The function definition is an expression, which describes how the output (return value) of this function will be calculated in the presence of valid inputs (function arguments).

The following list gives some examples of Haskell functions as lambda abstractions and their corresponding mathematical functions.

|    Haskell Function     | Mathematical Equivalence |
| :---------------------: | :----------------------: |
|   `(\x -> 2 * x + 1)`   |      f(x) = 2x + 1       |
|    `(\x y -> x + y)`    |     f(x, y) = x + y      |
| `(\a b c -> a + b - c)` |  f(a, b, c) = a + b - c  |

Passing argument(s) to a function (function application) is simply separating them with space(s), and the output of a function can be manually computed by substituting all the occurrences of corresponding variables in the function definition with the arguments, e.g.

```
(\x -> 2 * x + 1) 3
  == 2 * 3 + 1

(\a b -> a * a + b * b) (1 + 2) 4
  == (1 + 2) * (1 + 2) + 4 * 4
```

Replace the missing parts (indicated with `??`) with matching substitution results in the following pseudo-code. One example is given.
