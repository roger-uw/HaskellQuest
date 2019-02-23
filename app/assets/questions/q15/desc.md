In Haskell, expressions of the same type can be put together in a list.

The basic way of creating a list is directly write it out like the following line of code.

`xs = [1, 2, 3, 4, 5]`

An empty list is written as `[]`.

A list containing expressions of type `a` has type `[a]`, so the `xs` above can be rewritten with a possible type signature as follows.

```
xs :: [Int]
xs = [1, 2, 3, 4, 5]
```

If the list is an increasing range of numbers, such as the `xs` above, it can also be written as follows.

`xs = [1..5] :: [Int]`

A range could be infinite, for example, the list of all natural numbers.

`nat = [0..]`

A decreasing range must be explicitly stated, otherwise an empty list will be produced.

`[5, 4..1] == [5, 4, 3, 2, 1]`

`[5..1] == []`

A value of type `String` is a list of `Char` values, where each `Char` value can be written as a character wrapped by single quotation marks. Wrapping it with double quotation marks is an easier way to write a `String` value in Haskell.

`"Haskell" == ['H', 'a', 's', 'k', 'e', 'l', 'l']`

_The type `String` is actually a synonym of `[Char]`_

List related functions:

- `(:) :: a -> [a] -> [a]`  
  Add an element to the head of a list.  
  `1 : [2, 3] == [1, 2, 3]`  
  `1 : 2 : 3 : [] == [1, 2, 3]`

- `(++) :: [a] -> [a] -> [a]`  
  Append two lists.  
  `[1, 2] ++ [3] == [1, 2, 3]`  
  `[] ++ [1, 2, 3] == [1, 2, 3]`  
  `[1, 2, 3] ++ [] == [1, 2, 3]`

- `head :: [a] -> a`  
  Get the first element of a list.  
  `head [1, 2, 3] == 1`  
  `head []` will lead to an error.

- `tail :: [a] -> [a]`  
  Get the rest of a list except the first element.  
  `tail [1, 2, 3] == [2, 3]`  
  `tail []` will lead to an error.

- `length :: [a] -> Int`  
  Get the length of a list.  
  `length [1, 2, 3] == 3`  
  `length [] == 0`

- `take :: Int -> [a] -> [a]`  
  Take the first `n` elements from a list, where `n` is given by its first argument.  
  `take 2 [1, 2, 3] == [1, 2]`  
  `take 5 [1, 2, 3] == [1, 2, 3]`  
  `take 0 [1, 2, 3] == []`

- `drop :: Int -> [a] -> [a]`  
  Drop the first `n` elements from a list and return the rest, where `n` is given by its first argument.  
  `drop 2 [1, 2, 3] == [3]`  
  `drop 5 [1, 2, 3] == []`  
  `drop 0 [1, 2, 3] == [1, 2, 3]`

Replace the missing parts (indicated with `??`) in the following code, so the expression evaluate to `True`.
