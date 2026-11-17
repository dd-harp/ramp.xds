# Change parameters (**MY**)

For the **MY** module for the \\s^{th}\\ species, change parameter
values named in `options`, leaving other values unchanged.

## Usage

``` r
change_MY_pars(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

This is designed to work for static parameters or for parameters that
are set up as ports, where the class of the bionomic object is "static."

Use advanced setup to modify dynamic bionomic ports.

## See also

[get_MY_pars](https://dd-harp.github.io/ramp.xds/reference/get_MY_pars.md),
[setup_MY_obj](https://dd-harp.github.io/ramp.xds/reference/setup_MY_obj.md),
[xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)
