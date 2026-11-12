# Setup initial values for `basicL` (**L**)

Set initial values for all the variables in the `basicL` module (**L**)
for the \\(s^{th}\\) species.

- [make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md)
  supplies default values

- The values in `options` overwrite the defaults

- To inspect variable names, use
  [get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
  or see the inline module documentation.

The initial values of \\(L\\) can be passed as `options$L`.

## Usage

``` r
# S3 method for class 'basicL'
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

an **`xds`** object

## See also

[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md)
