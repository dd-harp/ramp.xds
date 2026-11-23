# Get the state variables for `SIS` (**XH**)

Extract the `SIS` variables from the state variables vector \\(y\\),
parse, and return the variables by name in a list.

## Usage

``` r
# S3 method for class 'SIS'
get_XH_vars(y, xds_obj, i)
```

## Arguments

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
