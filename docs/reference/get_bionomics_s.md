# Compute dynamical terms

Using the output of deSolve compute the dynamical terms for the output
of `xde_solve.ode` or `xde_solve.dde`

## Usage

``` r
get_bionomics_s(tm, de_vars, pars, s)
```

## Arguments

- tm:

  the time

- de_vars:

  the output of deSolve

- pars:

  an `xds` object

- s:

  a vector species index

## Value

[list](https://rdrr.io/r/base/list.html)
