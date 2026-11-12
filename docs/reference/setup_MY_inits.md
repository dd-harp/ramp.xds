# Setup initial values (**MY**)

Set initial values for all the variables.

- Each module (*MYname*) defines a function `make_MY_inits_`*MYname*
  that supplies default values

- The values in `options` overwrite the defaults

- To inspect variable names, use
  [get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
  or see the inline module documentation.

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

[list](https://rdrr.io/r/base/list.html)

## See also

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md),
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)
