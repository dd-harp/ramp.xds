# Static model for mosquito survival

Implements [F_mu](https://dd-harp.github.io/ramp.xds/reference/F_mu.md)
for a static model

## Usage

``` r
# S3 method for static
F_mu(t, vars, mu_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- mu_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
