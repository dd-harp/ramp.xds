# Times Utility for Discrete Time Systems

This method sets up the time points when output is wanted. For discrete
time systems, the system must compute values at every point, but it is
possible

If `times` is not null, it sets `maxT` to the maximum value in times.
Otherwise it sets `maxT=Tmax.`

It returns a sequence from `0` to `maxT` by `dt.`

A check values of `times` should be some integer multiple of `dt`

## Usage

``` r
make_times_dts(Tmax = 365, dt = 1, times = NULL)
```

## Arguments

- Tmax:

  the last time point, run from 0...Tmax

- dt:

  the time interval for outputs

- times:

  the times

## Value

an **`xds`** object
