# Set up initial values for `hMoI` (**XH**)

Set up initial values for the `hMoI` module using
[make_XH_inits_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_hMoI.md).
Values in `options` override the defaults. If `H` is supplied in
`options`, also update the module's population density.

## Usage

``` r
# S3 method for class 'hMoI'
setup_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- i:

  the host species index

## Value

an **`xds`** model object

## Note

Initial values are stored at `xds_obj$XH_obj[[i]]$inits`.

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)

[change_XH_inits.hMoI](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.hMoI.md)

[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
