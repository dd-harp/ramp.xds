# Set up the module object for `trivialXH` (**XH**)

This sets up a `trivialXH` model object for the **XH** module for the
\\(i^{th}\\) host species

- the object is created by
  [make_XH_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_trivial.md)

- `options` is passed to override the defaults

There are no dynamical variables requiring initial values;
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
leaves the object unmodified.

## Usage

``` r
# S3 method for class 'trivialXH'
setup_XH_obj(XHname, residence, xds_obj, options = list(), i = 1)

# S3 method for class 'trivial'
setup_XH_obj(XHname, residence, xds_obj, options = list(), i = 1)
```

## Arguments

- XHname:

  the model name

- residence:

  the residence vector

- xds_obj:

  an **`xds`** model object

- options:

  model options as a named list

- i:

  the host species index

## Value

an **`xds`** model object

## Note

The module object is stored at `xds_obj$XH_obj[[i]]` The compatibility
name `"trivial"` creates the same `trivialXH` object.

## See also

[trivialXH](https://dd-harp.github.io/ramp.xds/reference/trivialXH.md),
[make_XH_obj_trivial](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_trivial.md)
