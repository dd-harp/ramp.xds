# Get initial values (**MY**)

Return the stored initial values for the selected **MY** module and
vector species.

## Usage

``` r
get_MY_inits(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values, or
`NULL` if the field is absent

## Note

Initial values are stored at `xds_obj$MY_obj[[s]]$inits`. If the field
is absent, as in the `trivialMY` module, return `NULL`. An empty list is
returned unchanged.

## See also

[change_MY_inits](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.md),
[setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)
