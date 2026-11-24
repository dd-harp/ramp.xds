# Get the state variables (**MY**)

Extract the state variables for the selected **MY** module and vector
species from `y` and return them by name in a list for use during model
solving.

## Usage

``` r
get_MY_vars(y, xds_obj, s)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of state variables and
module-specific derived quantities

## Note

Dispatch uses the class of `xds_obj$MY_obj[[s]]`. These functions
support solver evaluations that need variables by name; users rarely
need to call them directly.
