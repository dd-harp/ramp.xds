# Compute the blood xieeding rate, xi

This method dispatches on the type of `xi_obj`. It should set the values
oxi the bionomic parameters to baseline values

## Usage

``` r
F_dlay_maturation(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector oxi length
`nPatches`
