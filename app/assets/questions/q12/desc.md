A variable can be bound to an lambda abstraction, e.g.

`square = (\x -> x * x)`

`mult = (\x y -> x * y)`

`isEqual = (\a b -> a == b)`

In this case, variables like `square` and `mult` are usually called functions.

The type of a lambda abstraction or a function has the following form:

`TypeOfParam0 -> TypeOfParam1 -> ... -> TypeOfReturnValue`

For example,

`square = (\x -> x * x) :: Int -> Int`

`mult = (\x y -> x * y) :: Int -> Int -> Int`

`isEqual = (\a b -> a == b) :: Int -> Int -> Bool`

And Haskell provides a easier way to write such definitions.

```
square :: Int -> Int
square x = x * x
```

```
mult :: Int -> Int -> Int
mult x y = x * y
```

```
isEqual :: Int -> Int -> Bool
isEqual a b = a == b
```

Actually, operators like `+`, `-`, `*`, `==` are just functions with infix syntax, which means they can appear between their arguments. An infix operator can be treated as a normal function when quoted with parentheses, and a normal function can be treated as an infix operator when wrapped with backquotes. Given below is an example.

`add = (+)`

`(+) 5 4 == add 5 4`

`` 5 + 4 == 5 `add` 4 ``

To make code easier to understand, sometimes a type synonym is given to a type as its alias.

`type Year = Int`

`type Month = Int`

`type Temperature = Double`

Replace the missing parts (indicated with `??`) in the following code, so the function `isInCircle` can be used to check in a Cartesian plane if a point `(pX, pY)` is inside a circle (excluding its boundary) defined by its centre `(cX, cY)` and radius `r`.
