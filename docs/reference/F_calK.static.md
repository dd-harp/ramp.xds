# Static model for mosquito emigration

Implements
[F_calK](https://dd-harp.github.io/ramp.xds/reference/F_calK.md) for a
static model

## Usage

``` r
# S3 method for static
F_calK(t, vars, calK_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- calK_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
