# Setup initial values for `trivial` (**MY**)

The `trivial` module (**MY**) for the \\s^{th}\\ species has no
dynamical variables requiring initial values. This method returns the
unmodified **`xds`** object; `options` is unused.

## Usage

``` r
# S3 method for class 'trivial'
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

an **`xds`** object
