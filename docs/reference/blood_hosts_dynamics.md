# blood_hosts Dynamics

on the `XY_interface.` Here is defined as time spent in one of the
patches in this model, so travle is time spent elsewhere.

A function `F_blood_hosts` computes `TiSp_frac_blood_hostsing` and this
function sets `TiSp_frac_here.`

## Usage

``` r
blood_hosts_dynamics(t, y, xds_obj)
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

## Note

The quantity `TiSp_frac_here` is part of the `XY_interface` It is used
to compute the Time at Risk matrix, and it is also used to weight local
exposure *vs.* blood_hostsing exposure.
