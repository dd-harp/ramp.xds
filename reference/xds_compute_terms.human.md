# Differential equations isolating the humans, forced with Ztrace

Compute derivatives for
[deSolve::ode](https://rdrr.io/pkg/deSolve/man/ode.html) or
[deSolve::dede](https://rdrr.io/pkg/deSolve/man/dede.html) using generic
methods for each model component.

## Usage

``` r
# S3 method for class 'human'
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
