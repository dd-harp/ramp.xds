# Setup initial values for `basicL` (**L**)

This sets initial values of the variable \\L\\ by calling
[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md).
Default values are used unless other values are passed in `options` by
name (*i.e.* `options$L`)

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
