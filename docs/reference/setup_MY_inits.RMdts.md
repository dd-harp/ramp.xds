# Set up initial values for `RMdts` (**MY**)

Set up initial values for the `RMdts` module using
[make_MY_inits_RMdts](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_RMdts.md).
Values in `options` override the defaults.

## Usage

``` r
# S3 method for class 'RMdts'
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

an **`xds`** model object

## Note

Initial values are stored at `xds_obj$MY_obj[[s]]$inits`.

## See also

[change_MY_inits.RMdts](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.RMdts.md)

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
