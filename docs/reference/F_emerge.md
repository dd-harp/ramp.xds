# Compute Emergent Adults

This function computes the rate or number of emerging adults: a rate for
differential equations, or a number for difference equations.

## Usage

``` r
F_emerge(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nHabitats`

## Note

This method dispatches on the class of `xds_obj$L_obj[[s]]`
