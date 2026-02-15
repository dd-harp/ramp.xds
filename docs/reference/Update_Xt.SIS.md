# DTS updating for the SIS model for human / vertebrate host infections

Implements
[Update_Xt](https://dd-harp.github.io/ramp.xds/reference/Update_Xt.md)
for the SIS model

## Usage

``` r
# S3 method for SIS
Update_Xt(t, y, pars, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
