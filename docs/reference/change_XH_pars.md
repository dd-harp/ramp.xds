# Change parameters (**XH**)

Change parameter values named in `options` for the selected **XH**
module for the \\(i^{th}\\) host species, leaving other values
unchanged.

## Usage

``` r
change_XH_pars(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the host species index

## Value

an **`xds`** model object

## Note

Dispatch is on the class of `xds_obj$XH_obj[[i]]`.

## See also

[get_XH_pars](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.md),
[setup_XH_obj](https://dd-harp.github.io/ramp.xds/reference/setup_XH_obj.md),
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_change_parameters.md)
