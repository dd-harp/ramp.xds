# Compute random emigration-loss fractions

Generate patch-level emigration-loss fractions using the configured
random number generator.

## Usage

``` r
# S3 method for class 'random'
F_mu(t, xds_obj, s)
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
