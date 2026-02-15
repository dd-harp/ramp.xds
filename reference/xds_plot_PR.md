# Plot the prevalence / parasite rate (PR) from a model of human infection and immunity

Plot the prevalence / parasite rate (PR) from a model of human infection
and immunity

## Usage

``` r
xds_plot_PR(
  xds_obj,
  method = "true",
  i = 1,
  clrs = "black",
  lty = 1,
  y01 = FALSE,
  add = FALSE
)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- method:

  the method used for computing *Pf*PR

- i:

  the host species index

- clrs:

  a vector of colors

- lty:

  an integer (or integers) that specifies `lty` for plotting

- y01:

  set ylim = c(0,1)

- add:

  a logical: plot axes only if FALSE

## Value

true pr, invisibly
