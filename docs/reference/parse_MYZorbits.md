# Parse the outputs and return the variables by name in a list

This method dispatches on the type of `pars$MYZpar`. It computes the
variables by name and returns a named list.

## Usage

``` r
parse_MYZorbits(outputs, pars, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of outputs from deSolve

- pars:

  a [list](https://rdrr.io/r/base/list.html) that defines a model

- s:

  the species index

## Value

[list](https://rdrr.io/r/base/list.html)
