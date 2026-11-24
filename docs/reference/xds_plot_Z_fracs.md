# Plot the infective mosquito fraction (**MY**)

Plot the saved infective mosquito fraction over time, one line per patch
for the selected species.

## Usage

``` r
xds_plot_Z_fracs(xds_obj, s = 1, clrs = "darkred", llty = 1, add = FALSE)
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

Reads `z = Z/M` and `time` through
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md);
it does not calculate the fraction. Creates a new plot unless
`add = TRUE`; then adds lines to the existing plot.

## See also

[xds_lines_Z_fracs](https://dd-harp.github.io/ramp.xds/reference/xds_lines_Z_fracs.md)
