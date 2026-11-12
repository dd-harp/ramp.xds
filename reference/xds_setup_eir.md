# Basic setup for host disease dynamics (EIR forced)

Build a dynamical system to describe human malaria epidemiology or host
disease dynamics forced by the daily EIR. The function encodes the model
as an **xds** object that can be solved by
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
or
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md).

To build a model that is forced by infective biting in the patches,
which requires setting up a time spent matrix and search weigths, use
[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md).

## Usage

``` r
xds_setup_eir(
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
  HPop = 1000,
  model_name = "unnamed"
)
```

## Arguments

- eir:

  is the average, daily entomological inoculation rate

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

- model_name:

  is a name for the model (arbitrary)

## Value

an **`xds`** object

## Details

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
that sets up a model to study exposure and infection. The **MY**
component module is set to `trivial` and a trace function computes
infective biting in the patches.

`class(xds_obj$frame) = 'eir'`

to dispatch functions that compute derivatives (`xde`), update variables
(`dts`), and parse outputs.

**Dynamics**

Configure the **XH** component:

- `XHname` — the name of an **XH** module: see the
  [XH_module_list](https://dd-harp.github.io/ramp.xds/reference/XH_module_list.md)
  for human / host infection dynamics

- `HPop` — the initial human population density

- `XHoptions` sets parameter values and initial conditions for the
  **XH** module

- `nHostSpecies`\\=1\\

The structural parameter `nStrata` is set to `length(HPop)`.

**The EIR Trace Function**

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

**Next**

- To solve the model, see
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md)
  or
  [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)

- To check or change parameters, see
  [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)

- To check or change the initial conditions, see
  [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)

- To modify the model structure, see
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_advanced_setup.md)

## See also

- Basic Setup:
  [xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
  [xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md),
  [xds_info_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_basic_setup.md)

  - [xds_info_change_parameters](https://dd-harp.github.io/ramp.xds/reference/xds_info_change_parameters.md)

  - [xds_info_initial_conditions](https://dd-harp.github.io/ramp.xds/reference/xds_info_initial_conditions.md)

- Advanced Setup:
  [xds_info_advanced_setup](https://dd-harp.github.io/ramp.xds/reference/xds_info_advanced_setup.md)

- Solving:
  [xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md),
  [xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)

- Internals:
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md),
  [xds_object](https://dd-harp.github.io/ramp.xds/reference/xds_object.md),
  [make_xds_object_template](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
