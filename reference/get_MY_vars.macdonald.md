# List variables for `macdonald` (**MY**)

This method dispatches on the type of `xds_obj$MY_obj[[s]]`

## Usage

``` r
# S3 method for class 'macdonald'
get_MY_vars(y, xds_obj, s)
```

## Arguments

- y:

  the variables

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

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
