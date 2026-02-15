# Parse the output of deSolve and return the variables by name in a list

This method dispatches on the type of `pars$Xpar[[i]]`. Adds the
variables from the X model to a list and returns it

## Usage

``` r
parse_Xorbits(outputs, pars, i)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of outputs from deSolve

- pars:

  an **`xds`** object

- i:

  the host species index
