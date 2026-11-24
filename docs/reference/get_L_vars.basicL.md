# Get the state variables for `basicL` (**L**)

Extract the `basicL` variables from the state variables vector \\(y\\),
parse, and return the variables by name in a list.

## Usage

``` r
# S3 method for class 'basicL'
get_L_vars(y, xds_obj, s)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html)
