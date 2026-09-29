# Compute random human feeding fractions

Generate patch-level human feeding fractions using the configured random
number generator.

## Usage

``` r
# S3 method for class 'random'
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
