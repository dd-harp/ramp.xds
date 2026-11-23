# Add lines for aquatic mosquito population density

Add aquatic mosquito population density lines for each habitat to an
existing plot.

## Usage

``` r
xds_lines_L(xds_obj, s = 1, clrs = "darkblue", llty = 1)
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

## Value

invisible(NULL)

## See also

[xds_plot_L](https://dd-harp.github.io/ramp.xds/reference/xds_plot_L.md)
