_I saw the footprints at the origin,_

_forgot where I was heading._

```
zero :: (a -> a) -> a -> a
zero = (\f x -> x)
```

_Under the glory of identity,_

_I have never been so complete._

```
one :: (a -> a) -> a -> a
one = (\f x -> f x)
```

_A sense of déjà vu,_

_where are we going,_

_what should I do?_

```
two :: (a -> a) -> a -> a
two = (\f x -> f (f x))
```

_Again, again,_

_tell me your wonderful plan._

```
three :: (a -> a) -> a -> a
three = (\f x -> f (f (f x)))
```

_There is no magic or trick,_

_just one more layer of shield._

```
succ :: ((a -> a) -> a -> a) ->
        ((a -> a) -> a -> a)
succ n = (\f x -> f (n f x))
```

_Embrace me with your warm arms,_

_covered by wounds and scars_

```
plus :: ((a -> a) -> a -> a) ->
        ((a -> a) -> a -> a) ->
        ((a -> a) -> a -> a)
plus a b = (\f x -> a f (b f x))
```

_Weave me deeply into your body,_

_so you'll never be lonely._

```
mult :: ((a -> a) -> a -> a) ->
        ((a -> a) -> a -> a) ->
        ((a -> a) -> a -> a)
mult a b = (\f x -> a (\z -> b f z) x)
```

_Quickly,_

_quickly,_

_the clock is ticking._

**Substitute,**

**substitute,**

_another way to calculate._
