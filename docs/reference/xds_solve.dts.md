# Solve a Discrete-Time System

Implements
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
for discrete time systems. Output times are set up or checked by
[make_times_dts](https://dd-harp.github.io/ramp.xds/reference/make_times_dts.md)
which guarantees time points are a unique set of integer values

## Usage

``` r
# S3 method for class 'dts'
xds_solve(xds_obj, Tmax = 365, dt = 1, times = NULL)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- Tmax:

  the last time point, run from 0...Tmax

- dt:

  the time step

- times:

  the times

## Value

an **`xds`** model object

## Note

For discrete time systems, the values in `times` need not be a sequence.
The system is solved for every time step. During parsing, the other
values are removed.
