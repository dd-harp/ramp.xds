# Get parameters (**XH**)

Inspect the parameter values for the **XH** module for the \\i^{th}\\
species

## Usage

``` r
get_XH_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

The parameter values are stored on `xds_obj$XH_obj[[i]]`

## See also

[change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md),
[setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md),
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)
