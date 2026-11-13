# Setup initial values for `trivial` (**L**)

The `trivial` module (**L**) for the \\s^{th}\\ species has no dynamical
variables requiring initial values. This method sets the initial values
to an empty list; `options` is unused.

## Usage

``` r
# S3 method for class 'trivial'
setup_L_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named [list](https://rdrr.io/r/base/list.html)

- s:

  the species index

## Value

an **`xds`** object
