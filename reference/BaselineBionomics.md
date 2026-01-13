# Set bionomic parameter rates relative to baseline

This calls MBaseline and LBaseline for each species.

This function sets bionomic parameters to their pre-control baseline
value, which can later be modified by vector control. In some models,
the pre-control baseline is computed in here as a function of resource
availability.

## Usage

``` r
BaselineBionomics(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

a [list](https://rdrr.io/r/base/list.html)
