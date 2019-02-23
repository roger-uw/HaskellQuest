A variable can be defined in Haskell by giving its name on the left hand side of an `=` and its definition, which is is usually an expression, on the right hand side. For example,

`x = 42`

This process of associating a variable with an expression is called _binding_.

The type of a variable can be declared before its definition, e.g.

`x :: Int`
`x = 42`

This is called a type signature and is optional in most cases.

The following list shows the diameters of five samples from a batch of glass marbles.

| Sample Index | Diameter (Unit: mm) |
| :----------: | :-----------------: |
|      0       |       `10.01`       |
|      1       |       `10.05`       |
|      2       |       `9.92`        |
|      3       |       `10.01`       |
|      4       |       `9.97`        |

Replace the missing parts (indicated with `??`) in the following code, so the value of the variable `result` indicates if the variance of measured diameters is less than `0.003`.

_In Haskell, an exponentiation where both the base `b` and the exponent `n` are real(floating-point) numbers can be written as `b ** n`_

_In Haskell, a variable name starts with a lowercase letter_
