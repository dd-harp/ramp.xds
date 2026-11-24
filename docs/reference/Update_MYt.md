# Update states (**MY**)

Compute the updated adult mosquito states for a discrete-time step using
the selected **MY** module.

## Usage

``` r
Update_MYt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of updated
states in the module's state-variable order; the current RMdts and SEI
methods return named lists instead

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.
