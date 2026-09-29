# Compute the mosquito patch emigration rate

This method dispatches on the type of `sigma_obj` and computes the patch
emigration rate, \\\sigma\\.

## Usage

``` r
F_sigma(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
