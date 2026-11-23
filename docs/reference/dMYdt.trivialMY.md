# Compute the derivatives for `trivialMY` (**MY**)

Return an empty derivative vector because the `trivialMY` module has no
state variables.

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
