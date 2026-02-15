# Compute the Mosquito Patch Emigration Rate

This method dispatches on the type of `sigma_obj`. It should set the
values the patch emigration rate, \\\sigma\\

## Usage

``` r
F_emigrate(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector osigma length
`nPatches`
