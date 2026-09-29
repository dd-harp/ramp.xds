# Compute the EIP

This method dispatches on the type of `eip_obj` and computes the
extrinsic incubation period.

## Usage

``` r
F_eip(t, xds_obj, s)
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
