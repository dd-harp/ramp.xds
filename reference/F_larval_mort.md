# Compute the blood phieeding rate, phi

This method dispatches on the type of `phi_obj`. It should set the
values ophi the bionomic parameters to baseline values

## Usage

``` r
F_larval_mort(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector ophi length
`nPatches`
