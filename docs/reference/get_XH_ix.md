# Get variable indices (**XH**)

Return the stored variable indices for the selected **XH** module and
host species.

## Usage

``` r
get_XH_ix(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of variable indices, or
`NULL` if the field is absent

## Note

Variable indices are stored at `xds_obj$XH_obj[[i]]$ix`. If the field is
absent, return `NULL`. An empty list is returned unchanged.

## See also

[setup_XH_ix](https://dd-harp.github.io/ramp.xds/reference/setup_XH_ix.md)
