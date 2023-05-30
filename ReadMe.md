# Hax

Single variable automatic differentiation

## Example

We use the function  $f(x)=\cos(\sin(x)+1)$

```haskell
module Main where

import Hax

main:: IO ()
main=let f=gcos $ (gsin gid) |+| (gconstant 1.0)
         df_dx=gradient f
         d2f_dx2=gradient df_dx
      print $ evaluate f 1.0
      print $ evaluate df_dx 1.0
      print $ evaluate d2f_dx2 1.0
```

