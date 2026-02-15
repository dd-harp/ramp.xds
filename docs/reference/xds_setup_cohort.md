# Build a Model of Human / Host Cohort Dynamics

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
to setup up studies of cohort dynamics.

The **`xds`** object defines `frame = class(frame) = 'cohort'` but there
is no `cohort` case for
[xds_solve](https://dd-harp.github.io/ramp.xds/reference/xds_solve.md).
Instead, cohort dynamics are studied using
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md),
which was designed to compare the outcomes for cohorts of different ages
when exposure is changing.

The interface includes options to configure a function describing
`F_eir` as a function of time, with seasonal components and a trend.
Exposure in a cohort is a function of its age, including a function that
modifies exposure by age.

## Usage

``` r
xds_setup_cohort(
  eir = 1,
  F_season = F_flat,
  season_par = list(),
  F_trend = F_flat,
  trend_par = list(),
  F_age = F_flat,
  age_par = list(),
  xds = "ode",
  Xname = "SIS",
  XHoptions = list(),
  HPop = 1000,
  searchB = 1,
  model_name = "unnamed"
)
```

## Arguments

- eir:

  is the entomological inoculation rate

- F_season:

  a function describing a seasonal pattern over time

- season_par:

  parameters to configure a seasonality function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- F_trend:

  a function describing a temporal trend over time

- trend_par:

  parameters to configure a trends function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- F_age:

  a assigning a biting weight by age

- age_par:

  parameters to configure an age weights function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- xds:

  is `ode` or `dde` or `dts` for ordinary OR delay differential OR
  difference equations

- Xname:

  is a character string specifying an **X** Component module

- XHoptions:

  a list to configure the **X** Component module

- HPop:

  is the number of humans in each stratum

- searchB:

  is a vector of search weights for blood feeding

- model_name:

  is a name for the model (arbitrary)

## Value

an **`xds`** object

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
and
[xds_setup_human](https://dd-harp.github.io/ramp.xds/reference/xds_setup_human.md)
and
[xds_solve_cohort](https://dd-harp.github.io/ramp.xds/reference/xds_solve_cohort.md)
