# Compute the blood feeding rate, f

This method dispatches on the type of `q_par$f_par`. It should set the
values of the bionomic parameters to baseline values.

## Usage

``` r
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
