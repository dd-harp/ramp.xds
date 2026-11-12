# Get initial values (**MY**)

Inspect the initial values for the **MY** module

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

[numeric](https://rdrr.io/r/base/numeric.html)

## Note

The initial values are stored as a named list called
`xds_obj$XH_obj[[i]]$inits`

## See also

[change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md),
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
