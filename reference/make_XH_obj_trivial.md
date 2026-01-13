# Make parameters for trivial human model

The trivial module configures forcing with a seasonal pattern

- \\H = F_H(t)\\ - human / host population density

- \\I = F_I(t)\\ - human / host infectious density

Since \\\kappa\\ is the ratio \\I/H\\

## Usage

``` r
make_XH_obj_trivial(
  nPatches,
  options,
  kappa = 0.1,
  HPop = 1,
  F_season = F_flat,
  season_par = list(),
  F_trend = F_flat,
  trend_par = list(),
  F_shock = F_flat,
  shock_par = list(),
  H_trend = F_flat,
  H_trend_par = list()
)
```

## Arguments

- nPatches:

  the number of patches

- options:

  a [list](https://rdrr.io/r/base/list.html)

- kappa:

  net infectiousness

- HPop:

  initial human population density

- F_season:

  a function describing a seasonal pattern

- season_par:

  parameters to configure a `F_season` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- F_trend:

  a function describing a temporal trend

- trend_par:

  parameters to configure `F_trend` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- F_shock:

  a function describing a temporal shock

- shock_par:

  parameters to configure `F_shock` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- H_trend:

  a function describing a temporal trend in human population density

- H_trend_par:

  parameters to configure `H_trend` using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

## Value

a [list](https://rdrr.io/r/base/list.html)
