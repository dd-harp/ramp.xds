# Compute the derivatives (**L**)

Compute the derivatives (**L**).

## Usage

``` r
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of derivatives;
empty for modules without state variables
