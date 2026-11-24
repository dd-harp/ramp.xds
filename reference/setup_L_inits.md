# Set up initial values (**L**)

Set up initial values for the selected **L** module and vector species.
Values in `options` override the defaults.

## Usage

``` r
setup_L_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named [list](https://rdrr.io/r/base/list.html)

- s:

  the species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$L_obj[[s]]`.

## See also

[get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md),
[xds_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)

[change_L_inits](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.md)
