# Compute Derivatives for **MY** module `GeM`

Compute the derivatives for the generalized, non-autonomous Macdonald
model for mosquito ecology and infection dynamics. See
[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md).

## Usage

``` r
# S3 method for class 'GeM'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
