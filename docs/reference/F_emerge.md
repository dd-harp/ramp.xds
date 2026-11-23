# Compute emergent adults (**L**)

This function computes the rate or number of emerging adults: a rate for
differential equations, or a number for discrete-time systems.

## Usage

``` r
F_emerge(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`
