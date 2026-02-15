# Compute Other Variables

The description for each method should include the equations.

## Usage

``` r
# S3 method for class 'setup'
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

  the i^th auxiliary variable

## Value

Derivatives for auxiliary variables as a
[vector](https://rdrr.io/r/base/vector.html)
