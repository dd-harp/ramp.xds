# Get the state variables (**XH**)

Extract the state variables for the selected **XH** module and host
species from `y` and return them by name in a list for use during model
solving.

## Usage

``` r
get_XH_vars(y, xds_obj, i = 1)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of state variables and
module-specific derived quantities

## Note

Dispatch uses the class of `xds_obj$XH_obj[[i]]`. These functions
support solver evaluations that need variables by name; users rarely
need to call them directly.
