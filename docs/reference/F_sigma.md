# Compute mosquito emigration rates

This method dispatches on the type of `q_par$sigma_par`. It should set
the values of sigma to (possibly changing) baseline value(s).

## Usage

``` r
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
