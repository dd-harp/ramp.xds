# Compute mosquito mortality

This method dispatches on the type of `g_obj`. It should compute the
baseline mosquito mortality rate, \\g\\

## Usage

``` r
F_g(t, xds_obj, s)
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
