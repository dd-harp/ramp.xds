# List variables for `SI` (**MY**)

This method dispatches on the type of `xds_obj$MY_obj[[s]]`

## Usage

``` r
# S3 method for class 'SI'
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

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md)
