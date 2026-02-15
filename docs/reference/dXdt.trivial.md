# Handle Derivatives for the `trivial` **X**-Module

The trivial model has no state variables so it returns a numeric vector
of length 0

## Usage

``` r
# S3 method for trivial
dXdt(t, y, pars, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
