# Time Spent Here

This function sets the value of a term that is the time spent *here,*
stored as `time_at_home` on the `XY_interface.` Here is defined as time
spent in one of the patches in this model, so travle is time spent
elsewhere.

A function `F_travel` computes the fraction of time spent traveling
stores its complement as `time_at_home.`

## Usage

``` r
travel_dynamics(t, y, xds_obj)
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

The quantity `time_at_home` is part of the `XY_interface` It is used to
compute the Time at Risk matrix, and it is also used to weight local
exposure *vs.* traveling exposure.
