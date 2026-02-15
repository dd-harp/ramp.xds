# Solve a Dynamical System

Besides solving, this function does several housekeeping tasks:

- runs
  [check_models](https://dd-harp.github.io/ramp.xds/reference/check_models.md)

- sets up the vector of times when outputs are wanted: see
  [make_times_xde](https://dd-harp.github.io/ramp.xds/reference/make_times_xde.md)
  or
  [make_times_dts](https://dd-harp.github.io/ramp.xds/reference/make_times_dts.md)

- sets up \\y_0,\\ the initial values vector (see
  [get_inits](https://dd-harp.github.io/ramp.xds/reference/get_inits.md))

- solves the system

- parses and attaches outputs (see
  [parse_outputs](https://dd-harp.github.io/ramp.xds/reference/parse_outputs.md))

## Usage

``` r
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

## Note

The function `xds_solve()` dispatches on `xds_obj$xde`
