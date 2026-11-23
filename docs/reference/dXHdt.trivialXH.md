# Compute the derivatives for `trivialXH` (**XH**)

The trivial model has no state variables so it returns a numeric vector
of length 0

## Usage

``` r
# S3 method for class 'trivialXH'
dXHdt(t, y, xds_obj, i)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
