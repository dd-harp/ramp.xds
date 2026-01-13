# Make parameters for trivial aquatic mosquito model

Make parameters for trivial aquatic mosquito model

## Usage

``` r
make_MY_obj_trivial(
  nPatches,
  options,
  f = 1,
  q = 1,
  Z = 1,
  eggs = 1,
  F_season = F_flat,
  season_par = list(),
  F_trend = F_flat,
  trend_par = list(),
  F_shock = F_flat,
  shock_par = list()
)
```

## Arguments

- nPatches:

  an integer

- options:

  a [list](https://rdrr.io/r/base/list.html) of values that overwrites
  the defaults

- f:

  the blood feeding rate

- q:

  the human fraction

- Z:

  the human fraction

- eggs:

  the human fraction

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

## Value

none
