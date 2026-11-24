# Get the state variables (**XH**)

Extract the **XH** variables from the state variables vector \\(y\\),
parse, and return the variables by name in a list.

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

Variables as a named list
