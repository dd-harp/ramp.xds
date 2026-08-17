# Make parameters for trivial human model

Make parameters for trivial human model

## Usage

``` r
make_XH_obj_trivial(
  nPatches,
  options,
  kappa = 0.1,
  HPop = 1,
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

  the number of patches

- options:

  a [list](https://rdrr.io/r/base/list.html)

- kappa:

  net infectiousness

- HPop:

  initial human population density

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

  a list of options for F_shock \#' @return a
  [list](https://rdrr.io/r/base/list.html)
