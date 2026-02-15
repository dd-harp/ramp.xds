# Solve a Discrete-Time System

Implements
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
for discrete time systems.

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

a [list](https://rdrr.io/r/base/list.html)

## Note

For discrete time systems, the values in `times` need not be a sequence.
If `times` is not null, the system iterates up to `max(times)` and then
returns the solutions at the subset specified in `times.`
