# Compute theta (density dependent mortality)

This method dispatches on the type of `theta_obj`. It should set the
values otheta the bionomic parameters to baseline values

## Usage

``` r
F_theta(t, xds_obj, s)
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
`nHabitats`
