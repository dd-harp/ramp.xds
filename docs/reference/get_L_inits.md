# Get initial values (**L**)

Inspect the initial values for the **L** module

## Usage

``` r
get_L_inits(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a named [list](https://rdrr.io/r/base/list.html)

## Note

The initial values are stored as a named list called
`xds_obj$L_obj[[s]]$inits`

## See also

[change_L_inits](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.md),
[setup_L_inits](https://dd-harp.github.io/ramp.xds/reference/setup_L_inits.md)
