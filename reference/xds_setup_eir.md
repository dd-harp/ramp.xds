# Basic setup for models of human/host disease dynamics forced by the EIR

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that sets up a model to study the outcomes of exposure, as defined by
the EIR. This sets `class(xds_obj$frame) = 'eir'` to dispatch functions
that compute derivatives (`xde`), update variables (`dts`), and parse
outputs for
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).
The model can also be solved by
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md).

Configure the **XH** component:

- `XHname` — human / host infection dynamics (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- `HPop` — the initial human population density

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

The structural parameter `nStrata` is set to `length(HPop)`.

The interface includes options to configure a composed, multiplicative
time series function describing `F_eir` as a function of time. For
cohort dynamics (using
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)),
a function can be configured to compute relative biting rates by age:

- `eir` — the average daily EIR (for each population stratum)

- `F_season` or `season_par` — set up the seasonal pattern function

- `F_trend` or `trend_par` — set up the trend pattern function

- `F_shock` or `shock_par` — set up the shock function

- `F_age` or `age_par` — set up relative biting rates by age

## Usage

``` r
xds_setup_eir(
  model_name = "unnamed",
  eir = 1/365,
  F_season = F_one,
  F_trend = F_one,
  F_shock = F_one,
  F_age = F_one,
  season_par = list(),
  trend_par = list(),
  shock_par = list(),
  age_par = list(),
  xds = "ode",
  XHname = "SIS",
  XHoptions = list(),
  HPop = 1000
)
```

## Arguments

- model_name:

  is a name for the model (arbitrary)

- eir:

  is the entomological inoculation rate

- F_season:

  the seasonal pattern function

- F_trend:

  the trend function

- F_shock:

  the shock function

- F_age:

  the relative biting rate by age

- season_par:

  a list of options for F_season

- trend_par:

  a list of options for F_trend

- shock_par:

  a list of options for F_shock

- age_par:

  a list of options for F_age

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- XHname:

  the name of an **XH** module (see
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md))

- XHoptions:

  a named list to set parameter values and initial conditions for the
  **XH** module

- HPop:

  the initial human population density for each stratum

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
