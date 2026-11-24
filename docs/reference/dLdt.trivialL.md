# Compute the derivatives for `trivialL` (**L**)

Return an empty numeric vector because the `trivialL` module has no
state variables.

## Usage

``` r
# S3 method for class 'trivialL'
dLdt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length 0
