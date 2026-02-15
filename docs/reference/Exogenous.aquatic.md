# Differential equation models for aquatic mosquito populations

Compute derivatives for
[deSolve::ode](https://rdrr.io/pkg/deSolve/man/ode.html) or
[deSolve::dede](https://rdrr.io/pkg/deSolve/man/dede.html) using generic
methods for each model component.

## Usage

``` r
# S3 method for aquatic
Exogenous(t, y, pars)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

## Value

a [list](https://rdrr.io/r/base/list.html) containing the vector of all
state derivatives
