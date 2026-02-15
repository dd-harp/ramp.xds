# Parse the output of deSolve and return variables for the trivial model

Implements
[parse_MYZorbits](https://dd-harp.github.io/ramp.xds/reference/parse_MYZorbits.md)
for trivial

## Usage

``` r
# S3 method for trivial
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
