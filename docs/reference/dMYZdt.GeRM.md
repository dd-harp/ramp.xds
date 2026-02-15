# **MYZ** Component Derivatives for the `GeRM` model

Compute the derivatives for the generalized, non-autonomous
Ross-Macdonald model for mosquito ecology and infection dynamics.

## Usage

``` r
# S3 method for GeRM
dMYZdt(t, y, pars, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an `xds` object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
