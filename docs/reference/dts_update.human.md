# Difference equations isolating the humans, forced with Ztrace

Compute and update the state variables for a model with only humans

## Usage

``` r
# S3 method for class 'human'
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

a [vector](https://rdrr.io/r/base/vector.html) containing the vector of
all state derivatives
