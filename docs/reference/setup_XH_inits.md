# Set up initial values (**XH**)

Set up initial values for the selected **XH** module and host species.
Values in `options` override the defaults.

## Usage

``` r
setup_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- i:

  the host species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$XH_obj[[i]]`.

## See also

[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md),
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)

[change_XH_inits](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.md)
