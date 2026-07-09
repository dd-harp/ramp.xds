# Analyze a Dynamical System

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
xds_analyze(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Note

The function
[`xds_solve()`](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
dispatches on `xds_obj$xde`
