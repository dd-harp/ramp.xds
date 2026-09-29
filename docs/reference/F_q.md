# Compute the human feeding fraction, q

This method dispatches on the type of `q_obj` and computes the human
feeding fraction, \\q\\.

## Usage

``` r
F_q(t, xds_obj, s)
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
