# Compute dynamical terms

Using the output of deSolve compute the dynamical terms for the output
of `xde_solve.ode` or `xde_solve.dde`

## Usage

``` r
get_terms(tm, de_vars, pars)
```

## Arguments

- tm:

  a vector of times

- de_vars:

  a matrix with the values of the variables

- pars:

  an **`xds`** object

## Value

[list](https://rdrr.io/r/base/list.html)
