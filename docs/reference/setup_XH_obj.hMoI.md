# Set up `hMoI` (**XH**)

This sets up an `hMoI` model object for the **XH** module for the
\\(i^{th}\\) host species

- the object is created by
  [make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md)

- the initial conditions are set by
  [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)

- `options` is passed to both to override the defaults

## Usage

``` r
# S3 method for class 'hMoI'
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

a [list](https://rdrr.io/r/base/list.html) vector

## Note

The module object is stored at `xds_obj$XH_obj[[i]]`

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md),
[make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md)
