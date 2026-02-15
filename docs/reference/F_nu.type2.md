# Type 2 functional response for the blood feeding rate

Implements [F_nu](https://dd-harp.github.io/ramp.xds/reference/F_nu.md)
for a static model

## Usage

``` r
# S3 method for type2
F_nu(t, vars, nu_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- nu_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
