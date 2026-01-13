# Generalized spatial differential equation model (mosquito only)

Mirrors
[xde_derivatives](https://dd-harp.github.io/ramp.xds/reference/xde_derivatives.md)
but only includes the adult and aquatic mosquito components.

## Usage

``` r
# S3 method for class 'mosy'
xde_derivatives(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

a [list](https://rdrr.io/r/base/list.html) containing the vector of all
state derivatives
