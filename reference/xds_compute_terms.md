# Compute Derivatives

The function to compute the derivatives dispatches on `class(frame)`

## Usage

``` r
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

## See also

[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
