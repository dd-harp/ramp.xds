# Static model for human blood fraction

Implements [F_q](https://dd-harp.github.io/ramp.xds/reference/F_q.md)
for a static model

## Usage

``` r
# S3 method for dynamic
F_q(t, vars, q_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- q_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
