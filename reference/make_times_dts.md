# Times Utility for Discrete Time Systems

This method sets up the time points when output is wanted. For
discrete-time systems, values must be computed at every time step, but
outputs can be returned only at selected times.

If `times` are provided, unique rounded values are returned.

Otherwise, it generates a sequence from `0` to `Tmax` by `dt`.

Provided values of `times` are rounded to integers. If `times` is
`NULL`, the sequence is rounded and only unique integer values are
returned.

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

a vector of time points
