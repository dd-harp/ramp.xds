# Compute the blood geeding rate, g

This method dispatches on the type of `g_obj`. It should set the values
og the bionomic parameters to baseline values

## Usage

``` r
F_mozy_mort(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector og length
`nPatches`
