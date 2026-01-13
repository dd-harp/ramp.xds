# Generalized spatial differential equation model (mosquito only)

Mirrors
[xds_compute_terms](https://dd-harp.github.io/ramp.xds/reference/xds_compute_terms.md)
but only includes the adult and aquatic mosquito components.

## Usage

``` r
# S3 method for class 'mosy'
xds_compute_terms(t, y, xds_obj)
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
