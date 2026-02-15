# Compute the egg laying rate

This method dispatches on the type of `q_par`. It should set the values
of nu to (possibly changing) baseline value(s).

## Usage

``` r
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
