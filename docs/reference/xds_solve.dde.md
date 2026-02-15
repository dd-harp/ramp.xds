# Solve a System of Delay Differential Equations

Implements
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
for delay differential equations

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

## Value

an **`xds`** object
