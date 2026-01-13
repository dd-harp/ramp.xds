# Compute the blood psieeding rate, psi

This method dispatches on the type of `psi_obj`. It should set the
values opsi the bionomic parameters to baseline values

## Usage

``` r
F_maturation(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector opsi length
`nPatches`
