# Compute the Egg Laying Rate

This method dispatches on the type of `nu_obj`. It should set the values
of the egg batch rate, \\\nu\\

## Usage

``` r
F_nu(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
