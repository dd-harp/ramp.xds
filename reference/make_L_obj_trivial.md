# Make the module object for `trivialL` (**L**)

The number of emerging adults is a function \$\$\Lambda F_S(t) F_T(t)
F_K(t)\$\$ where

- \\\Lambda\\ or `Lambda` is the mean number of adult female mosquitoes
  emerging from each aquatic habitat per day

- \\F_S(t)\\ or `F_season` is a seasonal pattern function (ideally, with
  an average annual mean of 1)

- \\F_T(t)\\ or `F_trend` is a trend pattern function (ideally, with an
  average value of 1)

- \\F_K(t)\\ or `F_shock` is a perturbation function

Use a supplied pattern function if passed, setting its corresponding
parameter object to an empty list. Otherwise, construct the function
from the corresponding parameter object using
[make_function](https://dd-harp.github.io/ramp.func/reference/make_function.html).

## Usage

``` r
make_L_obj_trivial(
  nHabitats,
  options = list(),
  Lambda = 100,
  F_season = list(),
  F_trend = list(),
  F_shock = list(),
  season_par = list(name = "F_one"),
  trend_par = list(name = "F_one"),
  shock_par = list(name = "F_one")
)
```

## Arguments

- nHabitats:

  the number of habitats in the model

- options:

  a [list](https://rdrr.io/r/base/list.html) that overrides default
  values

- Lambda:

  vector of mean emergence rates from each aquatic habitat

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

a [list](https://rdrr.io/r/base/list.html): an L module object
