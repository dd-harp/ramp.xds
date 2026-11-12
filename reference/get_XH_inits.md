# Get initial values (**XH**)

Inspect the initial values for the **XH** module

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

the initial values, as a named list

## Note

The initial values are stored as a named list called
`xds_obj$XH_obj[[i]]$inits`

## See also

[change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md),
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
