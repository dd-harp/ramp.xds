# Add lines for adult mosquito density (**MY**)

Add saved adult mosquito density lines to an existing plot, one line per
patch for the selected species.

## Usage

``` r
xds_lines_M(xds_obj, s = 1, clrs = "darkblue", llty = 1)
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

Reads `M` and `time` through
[get_MY_orbits](https://dd-harp.github.io/ramp.xds/reference/get_MY_orbits.md).
An existing plot is required.

## See also

[xds_plot_M](https://dd-harp.github.io/ramp.xds/reference/xds_plot_M.md)
