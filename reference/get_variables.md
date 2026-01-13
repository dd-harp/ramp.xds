# Get Variables

A utility to write functions that respond to state variables or
exogenous variables

## Usage

``` r
get_variables(t, y, func, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  variables

- func:

  a function that dispatches get_variables and pulls variables it needs

- xds_obj:

  an **`xds`** model object

## Value

a vector of variables

## Note

The method dispatches on `class(func)`
