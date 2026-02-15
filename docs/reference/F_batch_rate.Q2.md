# Type 2 Functional Response for Egg Laying

Implements
[F_batch_rate](https://dd-harp.github.io/ramp.xds/reference/F_batch_rate.md)
for a static model

## Usage

``` r
# S3 method for class 'Q2'
F_batch_rate(t, xds_obj, s)
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
