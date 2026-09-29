# Compute random extrinsic incubation periods

Generate patch-level EIP values with the configured positive random
number generator.

## Usage

``` r
# S3 method for class 'random'
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
