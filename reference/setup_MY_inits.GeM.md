# Setup initial values for `GeM` (**MY**)

Set initial values for all the variables in the `GeM` module (**MY**)
for the \\s^{th}\\ species.

- [make_MY_inits_GeM](https://dd-harp.github.io/ramp.xds/reference/make_MY_inits_GeM.md)
  supplies default values

- The values in `options` override the defaults

- To inspect variable names, use
  [get_MY_inits](https://dd-harp.github.io/ramp.xds/reference/get_MY_inits.md)
  or see the inline module documentation.

## Usage

``` r
# S3 method for class 'GeM'
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

a [list](https://rdrr.io/r/base/list.html)
