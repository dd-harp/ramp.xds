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

the initial values, as a named list

## Note

The initial values are stored as a named list called
`xds_obj$MY_obj[[s]]$inits`

## See also

[change_MY_inits](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.md),
[setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)
