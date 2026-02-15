# Differential equation models for human cohorts

Compute derivatives for
[deSolve::ode](https://rdrr.io/pkg/deSolve/man/ode.html) or
[deSolve::dede](https://rdrr.io/pkg/deSolve/man/dede.html) using generic
methods for each model component.

## Usage

``` r
xde_cohort_derivatives(age, y, xds_obj, birthday)
```

## Arguments

- age:

  host age

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

- birthday:

  the cohort birthday

## Value

a [list](https://rdrr.io/r/base/list.html) containing the vector of all
state derivatives
