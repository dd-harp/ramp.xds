# Solve a System of Delay Differential Equations

Solves a system of delay differential equations and computes the values
of the dependent variables.

Output times are set by
[make_times_xde](https://dd-harp.github.io/ramp.xds/reference/make_times_xde.md).

The returned model includes parsed outputs.

## Usage

``` r
# S3 method for class 'dde'
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
