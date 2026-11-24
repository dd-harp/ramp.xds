# Add lines for the infective mosquito fraction (**MY**)

Add saved infective mosquito fraction lines to an existing plot, one
line per patch for the selected species.

## Usage

``` r
xds_lines_Z_fracs(xds_obj, s = 1, clrs = "darkred", llty = 1)
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

## Value

invisible(NULL)

## Note

Reads `z = Z/M` and `time` through
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md);
it does not calculate the fraction. An existing plot is required.

## See also

[xds_plot_Z_fracs](https://dd-harp.github.io/ramp.xds/reference/xds_plot_Z_fracs.md)
