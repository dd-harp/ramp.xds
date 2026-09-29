# Compute the emigration loss fraction

This method dispatches on the type of `mu_obj`. It should compute the
baseline emigration-loss fraction, \\\mu\\

## Usage

``` r
F_mu(t, xds_obj, s)
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
