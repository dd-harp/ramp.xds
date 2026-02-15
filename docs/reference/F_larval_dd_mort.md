# Compute the blood thetaeeding rate, theta

This method dispatches on the type of `theta_obj`. It should set the
values otheta the bionomic parameters to baseline values

## Usage

``` r
F_larval_dd_mort(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector otheta length
`nPatches`
