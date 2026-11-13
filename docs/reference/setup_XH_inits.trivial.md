# Setup initial values for `trivial` (**XH**)

The `trivial` module (**XH**) for the \\(i^{th}\\) species has no
dynamical variables requiring initial values. This method returns the
unmodified **`xds`** object; `options` is unused.

## Usage

``` r
# S3 method for class 'trivial'
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
