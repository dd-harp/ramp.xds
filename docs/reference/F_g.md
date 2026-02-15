# Compute mosguito survival

This method dispatches on the type of `q_par$g_par`. It should set the
values of g to (possibly changing) baseline values.

## Usage

``` r
F_g(t, vars, g_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- g_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
