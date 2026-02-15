# Plot the annualized EIR *vs.* time

Plot the annualized EIR *vs.* time

## Usage

``` r
xds_plot_aEIR(
  xds_obj,
  i = 1,
  clrs = "black",
  llty = 1,
  stable = FALSE,
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

- stable:

  a logical: set to FALSE for `orbits` and FALSE for `stable_orbits`

- add:

  a logical: plot axes only if FALSE
