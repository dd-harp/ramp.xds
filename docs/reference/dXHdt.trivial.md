# Handle Derivatives for the `trivial` **X**-Module

The trivial model has no state variables so it returns a numeric vector
of length 0

## Usage

``` r
# S3 method for class 'trivial'
dXHdt(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
