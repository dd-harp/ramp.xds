# Make parameters for trivial human model

This builds a forcing function as a *composed time series function*
using either functions or parameters:

- Use a function if passed, and set the function object to an empty list

- If no function is passed, the parameters should be a function object
  (see
  [ramp.func::F_obj](https://dd-harp.github.io/ramp.func/reference/F_obj.html))

- Use `make_function` to create a function \\F(t,V)\\

## Usage

``` r
make_XH_obj_trivial(
  nPatches,
  options,
  kappa = 0,
  H = 1,
  F_season = list(),
  F_trend = list(),
  F_shock = list(),
  season_par = list(name = "F_one"),
  trend_par = list(name = "F_one"),
  shock_par = list(name = "F_one"),
  H_season = list(),
  H_trend = list(),
  H_shock = list(),
  H_season_par = list(name = "F_one"),
  H_trend_par = list(name = "F_one"),
  H_shock_par = list(name = "F_one")
)
```

## Arguments

- nPatches:

  the number of population strata (legacy argument name)

- options:

  a [list](https://rdrr.io/r/base/list.html)

- kappa:

  net infectiousness (mean value)

- H:

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

  a list of options for F_shock

- H_season:

  the seasonal pattern function for \\H\\

- H_trend:

  the trend function for \\H\\

- H_shock:

  the shock function for \\H\\

- H_season_par:

  a list of options for H_season

- H_trend_par:

  a list of options for H_trend

- H_shock_par:

  a list of options for H_shock

## Value

a [list](https://rdrr.io/r/base/list.html)

## See also

[ramp.func::F_obj](https://dd-harp.github.io/ramp.func/reference/F_obj.html)
