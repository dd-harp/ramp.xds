# Get variable indices (**MY**)

Return the stored variable indices for the selected **MY** module and
vector species.

## Usage

``` r
get_MY_ix(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of variable indices, or
`NULL` if the field is absent

## Note

Variable indices are stored at `xds_obj$MY_obj[[s]]$ix`. If the field is
absent, return `NULL`. An empty list is returned unchanged.

## See also

[setup_MY_ix](https://dd-harp.github.io/ramp.xds/reference/setup_MY_ix.md)
