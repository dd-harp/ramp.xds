# Plot the Temporal Trend

For a model with temporal forcing, show the temporal trend

## Usage

``` r
show_trend(
  xds_obj,
  tm = 10 * c(0:365),
  add = FALSE,
  clr = "black",
  rng = NULL,
  shock = TRUE
)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- tm:

  the time points

- add:

  add to existing plot

- clr:

  the line color

- rng:

  if not NULL, range limits for plotting

- shock:

  if 0,

## Value

the temporal trend, invisibly
