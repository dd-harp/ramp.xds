# Set up initial values for `SI` (**MY**)

Set up initial values for the `SI` module using
[make_MY_inits_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_SI.md).
Values in `options` override the defaults.

## Usage

``` r
# S3 method for class 'SI'
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

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[make_MY_inits_SI](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_SI.md)

[change_MY_inits.SI](https://dd-harp.github.io/ramp.xds/reference/change_MY_inits.SI.md)

[get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
