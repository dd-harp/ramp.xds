# Set up initial values for `basicL` (**L**)

Set up initial values for the `basicL` module using
[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md).
Values in `options` override the defaults.

The initial values of \\L\\ can be passed as `options$L`.

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

an **`xds`** model object

## Note

Initial values are stored at `xds_obj$L_obj[[s]]$inits`.

## See also

[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md)

[change_L_inits.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_inits.basicL.md)

[get_L_inits](https://dd-harp.github.io/ramp.xds/reference/get_L_inits.md)
