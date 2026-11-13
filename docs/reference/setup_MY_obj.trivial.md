# Set up `trivial` (**MY**)

This sets up a `trivial` model object for the **MY** module for the
\\(s^{th}\\) vector species

- the object is created by
  [make_MY_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_trivial.md)

- `options` is passed to override the defaults

In general, this should be used for aquatic mosquito ecology or human /
host epidemiology. In the former case, the user configures `F_eggs`; in
the latter, `F_fqZ`

## Usage

``` r
# S3 method for class 'trivial'
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

an **`xds`** object

## Note

The module object is stored at `xds_obj$MY_obj[[s]]`

## See also

[trivial_MY](https://dd-harp.github.io/ramp.xds/reference/trivial_MY.md),
[make_MY_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_MY_obj_trivial.md)
