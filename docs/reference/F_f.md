# Compute the blood feeding rate, f

Set the baseline value of the feeding rate, \\f\\.

## Usage

``` r
F_f(t, xds_obj, s)
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
