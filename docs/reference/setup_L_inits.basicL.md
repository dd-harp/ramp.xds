# Setup Initial Values for `basicL` (**L** Component)

This sets initial values of the variable \\L\\ by calling
[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md).
Default values are used unless other values are passed in `options` by
name (*i.e.* `options$L`)

## Usage

``` r
# S3 method for class 'basicL'
setup_L_inits(xds_obj, s, options = list())
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the species index

- options:

  a [list](https://rdrr.io/r/base/list.html)

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[make_L_inits_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_inits_basicL.md)
