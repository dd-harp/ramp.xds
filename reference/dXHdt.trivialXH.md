# Compute the derivatives for `trivialXH` (**XH**)

Return an empty numeric vector because the `trivialXH` module has no
state variables.

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
