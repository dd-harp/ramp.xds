# Habitats Dynamics

A function `F_habitat_weights` computes `TiSp_frac_searching` and this
function sets `TiSp_frac_here.`

## Usage

``` r
habitat_dynamics(t, y, xds_obj)
```

## Arguments

- t:

  current time

- y:

  state variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object

## Note

The quantity `TiSp_frac_here` is part of the `ML_interface` It is used
to compute the Time at Risk matrix, and it is also used to weight local
exposure *vs.* searching exposure.
