# Update states for discrete-time systems

Update the state variables for a *full* model.

## Usage

``` r
# S3 method for class 'full'
dts_update(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

updated dependent variables (a
[vector](https://rdrr.io/r/base/vector.html))
