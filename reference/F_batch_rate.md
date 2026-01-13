# Compute the Mosquito Patch Emigration Rate

This method dispatches on the type of `nu_obj`. It should set the values
the patch emigration rate, \\\nu\\

## Usage

``` r
F_batch_rate(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector onu length
`nPatches`
