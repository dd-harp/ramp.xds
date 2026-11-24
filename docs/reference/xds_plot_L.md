# Plot aquatic mosquito population density

Plot aquatic mosquito population density over time for each habitat.

## Usage

``` r
xds_plot_L(xds_obj, s = 1, clrs = "darkblue", llty = 1, add = FALSE)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

- clrs:

  a color for all habitats or a vector of colors, one per habitat

- llty:

  a line type for all habitats or a vector of line types, one per
  habitat, passed to `lty`

- add:

  add lines to the existing plot if TRUE

## Value

invisible(NULL)

## See also

[xds_lines_L](https://dd-harp.github.io/ramp.xds/reference/xds_lines_L.md)
