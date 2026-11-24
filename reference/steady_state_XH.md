# Compute steady states (**XH**)

Compute steady-state host and infection densities for the selected
**XH** module given daily FoI `foi`.

## Usage

``` r
steady_state_XH(foi, xds_obj, i = 1)
```

## Arguments

- foi:

  the daily FoI

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

module-specific steady-state values; see the selected method for the
return type and state names

## Note

Dispatch on the module object at `xds_obj$XH_obj[[i]]`. A
module-specific method must be supplied to use this generic. Return
values are not stored on the model object.
