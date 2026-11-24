# Get parameters (**MY**)

Return the parameters for the selected **MY** module for the \\s^{th}\\
vector species as a named list.

## Usage

``` r
get_MY_pars(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of parameter values

## Note

Parameter values are stored on `xds_obj$MY_obj[[s]]`. Dispatch is on the
class of `xds_obj$MY_obj[[s]]`.

## See also

[change_MY_pars](https://dd-harp.github.io/ramp.xds/reference/change_MY_pars.md),
[setup_MY_obj](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.md),
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_change_parameters.md)
