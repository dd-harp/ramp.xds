# Setup initial values for `hMoI` (**XH**)

Set initial values for all the variables in the `hMoI` module (**XH**)
for the \\(i^{th}\\) species.

- [make_XH_inits_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_hMoI.md)
  supplies default values

- The values in `options` overwrite the defaults

- To inspect variable names, use
  [get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
  or see the inline module documentation.

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

a [list](https://rdrr.io/r/base/list.html) vector
