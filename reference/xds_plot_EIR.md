# Plot the EIR *vs.* time

Plot the EIR *vs.* time

## Usage

``` r
xds_plot_EIR(
  xds_obj,
  i = 1,
  clrs = "black",
  llty = 1,
  add = FALSE,
  annual = TRUE
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

  a logical: plot axes only if FALSE

- annual:

  if true, plot as an annualized rate

## Value

eir, invisibly
