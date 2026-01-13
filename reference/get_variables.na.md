# Get Variables

The null case for
[get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)

## Usage

``` r
# S3 method for class 'na'
get_variables(t, y, func, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  variables

- func:

  a function that dispatches
  [get_variables](https://dd-harp.github.io/ramp.xds/reference/get_variables.md)
  and pulls variables it needs

- xds_obj:

  an **`xds`** model object

## Value

a set of variables
