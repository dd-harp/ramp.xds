# Plot the Temporal shock

For a model with temporal forcing, show the temporal shock

## Usage

``` r
show_shock(xds_obj, tm = 10 * c(0:365), add = FALSE, clr = "black", rng = NULL)
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

## Value

the temporal shock, invisibly
