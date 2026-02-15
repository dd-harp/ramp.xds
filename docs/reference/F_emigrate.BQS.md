# Model for mosquito emigration based on resource availability

Implements
[F_emigrate](https://dd-harp.github.io/ramp.xds/reference/F_emigrate.md)
for a static model

## Usage

``` r
# S3 method for class 'BQS'
F_emigrate(t, xds_obj, s)
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
