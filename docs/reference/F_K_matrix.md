# Compute the blood feeding rate, f

It should set the values of the bionomic parameters to baseline values

## Usage

``` r
F_K_matrix(t, xds_obj, s)
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

## Note

This method dispatches on the type of `f_obj` attached to the `MY_obj`.
