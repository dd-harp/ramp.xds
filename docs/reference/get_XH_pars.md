# Get parameters (**XH**)

Return the parameters for the selected **XH** module for the
\\(i^{th}\\) host species as a named list.

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

a named [list](https://rdrr.io/r/base/list.html) of parameter values

## Note

The parameter values are stored on `xds_obj$XH_obj[[i]]`. Dispatch is on
the class of `xds_obj$XH_obj[[i]]`.

## See also

[change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md),
[setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md),
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_change_parameters.md)
