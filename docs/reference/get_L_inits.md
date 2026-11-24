# Get initial values (**L**)

Return the stored initial values for the selected **L** module and
vector species.

## Usage

``` r
get_L_inits(xds_obj, s = 1)
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

Initial values are stored at `xds_obj$L_obj[[s]]$inits`. An empty list,
as in the `trivialL` module, is returned unchanged. If the field is
absent, return `NULL`.

## See also

[change_L_inits](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.md),
[setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md)
