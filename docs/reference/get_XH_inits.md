# Get initial values (**XH**)

Return the stored initial values for the selected **XH** module and host
species.

## Usage

``` r
get_XH_inits(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values, or
`NULL` if the field is absent

## Note

Initial values are stored at `xds_obj$XH_obj[[i]]$inits`. If the field
is absent, as in the `trivialXH` module, return `NULL`. An empty list is
returned unchanged.

## See also

[change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md),
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
