# Parse outputs for basicM

Implements
[parse_MYZorbits](https://dd-harp.github.io/ramp.xds/reference/parse_MYZorbits.md)
for the basicM model.

## Usage

``` r
# S3 method for basicM
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
