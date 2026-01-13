# Compute **XH** Component Derivatives

Using the stored values of the daily FoI, compute the derivatives and
return the derivatives as a numeric vector.

## Usage

``` r
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

the derivatives as a [numeric](https://rdrr.io/r/base/numeric.html)
vector

## Note

This method dispatches on `class(xds_obj$XH_obj)`
