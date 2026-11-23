# Compute the baseline blood feeding rate (**MY**)

Compute the baseline blood feeding rate, \\(f\\).

## Usage

``` r
F_f(t, xds_obj, s)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Note

This method dispatches on the class of the port object `f_obj` attached
to the `MY_obj`.
