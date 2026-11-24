# Compute steady states (**M**)

Compute steady-state mosquito densities for the selected **M** component
given adult mosquito emergence `Lambda`.

## Usage

``` r
steady_state_M(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

module-specific steady-state values; see the selected method for the
return type and state names

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`. Equation-system
selection and supported steady-state calculations depend on the module.
Return values are not stored on the model object.
