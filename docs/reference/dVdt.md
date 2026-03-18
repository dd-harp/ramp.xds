# Compute Other State Variables

The description for each method should include the equations.

## Usage

``` r
dVdt(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the i\\^{th}\\ set of auxiliary variables

## Value

derivatives for the \\\cal MYZ\\ component as a
[vector](https://rdrr.io/r/base/vector.html)

## Note

This is the `S3` generic. Methods dispatch on `V_obj`
