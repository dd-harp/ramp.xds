# Compute Derivatives for **MY** module `GeRM`

Compute the derivatives for the generalized, non-autonomous
Ross-Macdonald model for mosquito ecology and infection dynamics.

## Usage

``` r
# S3 method for class 'GeRM'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
