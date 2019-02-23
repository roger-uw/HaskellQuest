Recall the behaviours of `(++) :: [a] -> [a] -> [a]`.

```
[1, 2] ++ [3] == [1, 2, 3]
[] ++ [1, 2, 3] == [1, 2, 3]
[1, 2, 3] ++ [] == [1, 2, 3]
```

The function `append` defined below has the same usage as `(&&)`. `append` is recursive because it uses itself inside its definition.

```
append [] x = x
append (x : xs) ys = x : append xs ys
```

The pseudo-code below shows the evaluation process of `append [1, 2, 3] [4, 5]`. Lists are written with `[]` and `(:)` to show the process more clearly.

```
   append (1 : 2 : 3 : []) (4 : 5 : [])
~> 1 : append (2 : 3 : []) (4 : 5 : [])
~> 1 : 2 : append (3 : []) (4 : 5 : [])
~> 1 : 2 : 3 : append [] (4 : 5 : [])
~> 1 : 2 : 3 : 4 : 5 : []
```

---

Another example for recursive definition is the `map` function defined below.

```
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs
```

`map` applies a function to each element of a list.

The pseudo-code below shows the evaluation process of `map (\x -> x + 1) [1, 2, 3]`. Lists are written with `[]` and `(:)` to show the process more clearly.

```
   map (\x -> x + 1) (1 : 2 : 3 : [])
~> (\x -> x + 1) 1 : map (\x -> x + 1) (2 : 3 : [])
~> (1 + 1) : map (\x -> x + 1) (2 : 3 : [])
~> 2 : map (\x -> x + 1) (2 : 3 : [])
~> 2 : (\x -> x + 1) 2 : map (\x -> x + 1) (3 : [])
~> 2 : (2 + 1) : map (\x -> x + 1) (3 : [])
~> 2 : 3 : map (\x -> x + 1) (3 : [])
~> 2 : 3 : (\x -> x + 1) 3 : map (\x -> x + 1) []
~> 2 : 3 : (3 + 1) : map (\x -> x + 1) []
~> 2 : 3 : 4 : map (\x -> x + 1) []
~> 2 : 3 : 4 : []
```

---

Replace the missing parts (indicated with `??`) with proper expressions in the following pseudo-code, so it shows a complete evaluation process of `fib 4`. The definition of `fib` is given below.

```
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```
