# Static model for the blood feeding rate

Implements [F_f](https://dd-harp.github.io/ramp.xds/reference/F_f.md)
for a static model

## Usage

``` r
# S3 method for static
F_f(t, vars, f_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- f_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
