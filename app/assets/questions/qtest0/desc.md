This is a test question.

The Fibonacci sequence `F(n)` is defined as

![Equation](__QPATH__/equation.svg 'Equation')

In Haskell, the definition above can be directly expressed as the following recursive function.

```
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

However, this implementation is usually considered to be inefficient.

Replace the missing parts (indicated with `??`) in the following code, so `fibs !! n` gives the `n`th element in the Fibonacci sequence efficiently.

The answer could be

```
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```

_Reference: [The Fibonacci sequence](https://wiki.haskell.org/The_Fibonacci_sequence)_
