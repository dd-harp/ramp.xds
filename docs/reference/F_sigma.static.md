# Static model for mosquito emigration

Implements
[F_sigma](https://dd-harp.github.io/ramp.xds/reference/F_sigma.md) for a
static model

## Usage

``` r
# S3 method for static
F_sigma(t, vars, sigma_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- sigma_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
