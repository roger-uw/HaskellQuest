One straightforward way of describing conditional computation in Haskell is the conditional expression with the following form.

`if cond then expression0 else expression1`

`cond` is an expression of type `Bool`. `expression0` and `expression1` must have the same type. Depending on the evaluation result of `cond`, the following relationships hold.

```
(if True then expression0 else expression1)
  == expression0
```

```
(if False then expression0 else expression1)
  == expression1
```

For example,

`(if 1 + 1 == 2 then "sure" else "wow") == "sure"`

---

Another way of describing conditional computation is using guards when defining a function.

```
functionName parameters
  | guard0 = expression0
  | guard1 = expression1
  ...
  | guardN = expressionN
```

A guard is an expression of type `Bool`, the guards will be checked one by one until a true guard is met, and then the successive expression will be treated as the result of current function.

For example:

```
f x
  | x < -3 = -3
  | x < 3 = x
  | otherwise = 3
```

The code above defines the following piecewise function.

![Function](__QPATH__/equation.svg 'Function')

`otherwise` is actually a variable defined as `otherwise = True` for better readability.

---

A convenient syntax called pattern matching can also be used to define functions for different forms of arguments.

For example, the following function `and` shows the basic usage of pattern matching. `and` has the same usage as `(&&)`.

```
and :: Bool -> Bool -> Bool
and False _ = False
and True  b = b
```

A value of type `Bool` can be either `True` or `False`, the first argument of `and` in a function application will be evaluated to see if it matches any one of them. If the first argument matches `False`, no matter what the second argument is, the result would be `False`. Thus, an underscore `_` that matches any pattern is used to indicate that the second argument is not important and not used. If the first argument matches `True`, the result would equal the second argument, so a variable `b`, which matches any pattern, is inserted at the place of the second parameter and used as the result.

Another example is the `head` function. `error` is a function that can report an error and stop the execution.

```
head [] = error "Empty List!"
head (x : _) = x
```

The list as the argument of `head` will firstly checked to see if it matches with `[]`, if it does, an error will be reported because we cannot get an element from `[]`. If the list is not empty, which means there is at least one element, the list will be decomposed to the form `(x : xs)` and `x` will be the result. Since `xs` does not appear in the result, it is replaced by an underscore.

Replace the missing parts (indicated with `??`) in the following code, so the function `isLeap :: Int -> Bool` can be used to tell if a year is a leap year.
