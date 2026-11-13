# Setup initial values (**XH**)

Set initial values for all the variables.

- Each module (*XHname*) defines a function `make_XH_inits_`*XHname*
  that supplies default values

- The values in `options` override the defaults

- To inspect variable names, use
  [get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
  or see the inline module documentation.

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

an **`xds`** object

## See also

[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md),
[xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)
