# Set up initial values (**L**)

Set initial values for all the variables.

- Each non-trivial module defines a module-specific initial-value
  constructor that supplies default values

- The values in `options` override the defaults

- To inspect variable names, use
  [get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
  or see the module documentation listed in
  [L_module_list](https://dd-harp.github.io/ramp.xds/reference/L_module_list.md).

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

## See also

[get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md),
[xds_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_initial_conditions.md)
