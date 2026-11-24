# Plot infected mosquito density (**MY**)

Plot saved infected mosquito density over time, one line per patch for
the selected species.

## Usage

``` r
xds_plot_Y(xds_obj, s = 1, clrs = "purple", llty = 1, add = FALSE)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

- clrs:

  a color for all patches or a vector of colors, one per patch

- llty:

  a line type for all patches or a vector of line types, one per patch,
  passed to `lty`

- add:

  add lines to the existing plot if `TRUE`

## Value

invisible(NULL)

## Note

Reads `Y` and `time` through
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md).
Creates a new plot unless `add = TRUE`; then adds lines to the existing
plot.

## See also

[xds_lines_Y](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Y.md)
