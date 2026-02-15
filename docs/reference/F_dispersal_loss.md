# Compute the emigration loss fraction

This method dispatches on the type of `mu_obj`. It should set the values
omu the bionomic parameters to baseline values

## Usage

``` r
F_dispersal_loss(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector omu length
`nPatches`
