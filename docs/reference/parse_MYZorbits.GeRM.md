# Parse the output of deSolve and return variables for the GeRM model

Implements
[parse_MYZorbits](https://dd-harp.github.io/ramp.xds/reference/parse_MYZorbits.md)
for the GeRM model

## Usage

``` r
# S3 method for GeRM
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

a [list](https://rdrr.io/r/base/list.html)
