# Compute the human blood fraction

This method dispatches on the type of `p_par`. It should set the values
of the bionomic parameters to baseline values.

## Usage

``` r
F_p(t, vars, p_par)
```

## Arguments

- t:

  current simulation time

- vars:

  exogenous variables

- p_par:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
