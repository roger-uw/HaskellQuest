The (in)equality of two Boolean values or two numbers can be tested with `==`, `/=`, `>`, `<`, `>=` and `<=`.

The following list gives the descriptions of the operators above. All of them take two expression of the same type and generate a result of type `Bool`.

|      |                                     |
| ---- | ----------------------------------- |
| `==` | ... is equal to ...                 |
| `/=` | ... is not equal to ...             |
| `>`  | ... is greater than ...             |
| `<`  | ... is less than ...                |
| `>=` | ... is greater than or equal to ... |
| `<=` | ... is less than or equal to ...    |

The following list shows the diameters of five samples from a batch of glass marbles.

| Sample Index | Diameter (Unit: mm) |
| :----------: | :-----------------: |
|      0       |       `10.01`       |
|      1       |       `10.05`       |
|      2       |       `9.92`        |
|      3       |       `10.01`       |
|      4       |       `9.97`        |

If the mean value of measured diameters is within the range from `9.95` mm to `10.05` mm (inclusive) and the variance is less than `0.003`, this batch of marbles is qualified.

Replace the missing parts (indicated with `??`) in the following code so its evaluation result indicates if this batch is qualified.

_Actually, `==` and `/=` can be used on any two values of type that supports equality test; `>`, `<`, `>=` and `<=` can be used on any two values of type that supports comparison in size_
