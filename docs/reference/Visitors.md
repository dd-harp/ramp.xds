# Visitors

This function sets the value of a parameter describing time spent
F_visitors. It is computed in
[compute_TaR](https://dd-harp.github.io/ramp.xds/reference/compute_TaR.md)
and used to compute availability. It is also used in
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)

## Usage

``` r
Visitors(t, y, xds_obj)
```

## Arguments

- t:

  current time

- y:

  the state variable vector

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
