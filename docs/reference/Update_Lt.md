# Update states (**L**)

Compute the updated aquatic mosquito states for a discrete-time step
using the selected **L** module.

## Usage

``` r
Update_Lt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of updated
states in the module's state-variable order; empty for modules without
state variables

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`.
