# Compute derivatives for `trivialMY` (**MY**)

Implements
[dMYdt](https://dd-harp.github.io/ramp.xds/reference/dMYdt.md) for the
trivial (forced emergence) model.

## Usage

``` r
# S3 method for class 'trivialMY'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length 0
