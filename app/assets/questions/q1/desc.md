In Haskell, there are two elementary values, a. k. a. the Boolean values, `True` and `False`. Three basic operations on Boolean values are logical AND, logical OR and logical NOT, which can be written as `&&`, `||` and `not`, respectively. The following rules apply in computations involving Boolean values.

|                          |                          |
| ------------------------ | ------------------------ |
| `not True = False`       | `not False = True`       |
| `True || True = True`    | `True && True = True`    |
| `False || True = True`   | `False && True = False`  |
| `False || False = False` | `False && False = False` |
| `True || False = True`   | `True && False = False`  |

Replace the missing parts (indicated with `??`) in the following code so it evaluates to True.
