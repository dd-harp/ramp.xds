# Compute dynamical terms

Using the output of deSolve compute the dynamical terms for the output
of `xde_solve.ode` or `xde_solve.dde`

## Usage

``` r
get_bionomics(tm, de_vars, pars)
```

## Arguments

- tm:

  the time

- de_vars:

  the output of deSolve

- pars:

  an `xds` object

## Value

a named [list](https://rdrr.io/r/base/list.html)
