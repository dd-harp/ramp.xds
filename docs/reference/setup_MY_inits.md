# Set up initial values (**MY**)

Set up initial values for the selected **MY** module and vector species.
Values in `options` override the defaults.

## Usage

``` r
setup_MY_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$MY_obj[[s]]`.

## See also

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)

[change_MY_inits](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.md)
