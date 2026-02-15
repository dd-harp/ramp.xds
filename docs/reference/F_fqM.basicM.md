# The net blood feeding rate of the mosquito population in a patch

Implements
[F_fqM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.md) for the
basicM xde model.

## Usage

``` r
# S3 method for class 'basicM'
F_fqM(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
