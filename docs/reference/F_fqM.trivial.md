# Mosquito biting rate for `trivial`

The trivial model does not track an adult mosquito population, so this
method returns an empty numeric vector.

## Usage

``` r
# S3 method for class 'trivial'
F_fqM(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length 0
