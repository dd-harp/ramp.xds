# Compute steady states (**L**)

Compute steady-state aquatic mosquito densities for the selected **L**
module given the daily egg input `eta` in each habitat.

## Usage

``` r
steady_state_L(eta, xds_obj, s = 1)
```

## Arguments

- eta:

  the number of eggs laid in each habitat, per day

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a named [list](https://rdrr.io/r/base/list.html): values of the state
variables at the steady state

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`. Equation-system
selection and supported steady-state calculations depend on the module.
Return values are not stored on the model object.
