# Plot adult mosquito density (**MY**)

Plot saved adult mosquito density over time, one line per patch for the
selected species.

## Usage

``` r
xds_plot_M(xds_obj, s = 1, clrs = "darkblue", llty = 1, add = FALSE)
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

Reads `M` and `time` through
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md).
Creates a new plot unless `add = TRUE`; then adds lines to the existing
plot.

## See also

[xds_lines_M](https://dd-harp.github.io/ramp.xds/reference/xds_lines_M.md)
