# Build a Model for a single Human / Host Epidemiology forced by the EIR

A modified version of
[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md)
to setup up studies of malaria epidemiology, defined in a narrow sense,
to examine patterns in populations forced by the EIR.

The **`xds`** object defines `frame = class(frame) = 'eir'` to dispatch
functions that compute derivatives (`xde`), update variables (`dts`),
and parse outputs

The interface includes options to configure a function describing
`F_eir` as a function of time, with seasonal components and a trend.

This can be used to model a cohort as it ages; a function is set up to
modify exposure by age.

## Usage

``` r
xds_setup_eir(
  eir = 1,
  season_par = makepar_F_one(),
  trend_par = makepar_F_one(),
  age_par = makepar_F_one(),
  shock_par = makepar_F_one(),
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

- season_par:

  parameters to configure a seasonality function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- trend_par:

  parameters to configure a trends function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- age_par:

  parameters to configure an age weights function using
  [make_function](https://dd-harp.github.io/ramp.xds/reference/make_function.md)

- shock_par:

  parameters to configure a shock using
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
