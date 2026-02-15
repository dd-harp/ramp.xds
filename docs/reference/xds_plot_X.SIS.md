# Plot the density of infected individuals for the SIS model

Plot the density of infected individuals for the SIS model

## Usage

``` r
# S3 method for class 'SIS'
xds_plot_X(
  xds_obj,
  i = 1,
  clrs = c("darkblue", "darkred"),
  llty = 1,
  add = FALSE
)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

- clrs:

  a vector of colors

- llty:

  an integer (or integers) to set the `lty` for plotting

- add:

  plot axes only if FALSE
