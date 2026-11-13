# Set up `macdonald` (**MY**)

This sets up a `macdonald` model object for the **MY** module for the
\\s^{th}\\ vector species

- the object is created by
  [make_MY_obj_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_macdonald.md)

- the initial conditions are set by
  [setup_MY_inits](https://dd-harp.github.io/ramp.xds/reference/setup_MY_inits.md)

- `options` is passed to both to override the defaults

## Usage

``` r
# S3 method for class 'macdonald'
setup_MY_obj(MYname, xds_obj, options = list(), s = 1)
```

## Arguments

- MYname:

  the name of the model

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

a [list](https://rdrr.io/r/base/list.html) vector

## Note

The module object is stored at `xds_obj$MY_obj[[s]]`

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[make_MY_obj_macdonald](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_macdonald.md)
