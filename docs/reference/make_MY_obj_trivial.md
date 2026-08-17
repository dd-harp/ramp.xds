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
  F_season = F_one,
  F_trend = F_one,
  F_shock = F_one,
  season_par = list(name = "F_one"),
  trend_par = list(name = "F_one"),
  shock_par = list(name = "F_one")
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

  the density of infectious mosquitoes

- eggs:

  the mean egg laying rate

- F_season:

  the seasonal pattern function

- F_trend:

  the trend function

- F_shock:

  the shock function

- season_par:

  a list of options for F_season

- trend_par:

  a list of options for F_trend

- shock_par:

  a list of options for F_shock

## Value

a [list](https://rdrr.io/r/base/list.html)
