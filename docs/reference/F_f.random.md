# Compute random blood feeding rates

Generate patch-level blood feeding rates using the configured random
number generator.

## Usage

``` r
# S3 method for class 'random'
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
