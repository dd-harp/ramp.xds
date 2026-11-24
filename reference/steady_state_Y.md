# Compute steady states (**Y**)

Compute steady-state infection densities for the selected **Y**
component given adult mosquito density `M` and net infectiousness
`kappa`.

## Usage

``` r
steady_state_Y(M, kappa, xds_obj, s = 1)
```

## Arguments

- M:

  adult mosquito population density

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

module-specific steady-state values; see the selected method for the
return type and state names

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`. A
module-specific method must be supplied to use this generic. Return
values are not stored on the model object.
