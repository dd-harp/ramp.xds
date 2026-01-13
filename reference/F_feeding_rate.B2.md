# Type 2 functional response for the blood feeding rate

Implements
[F_feeding_rate](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.md)
for a static model

## Usage

``` r
# S3 method for class 'B2'
F_feeding_rate(t, xds_obj, s)
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
