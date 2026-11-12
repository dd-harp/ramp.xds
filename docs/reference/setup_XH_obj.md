# Set up an **XH** model object

Set up creates an **XH** object for the \\(i^{th}\\) species and sets up
the interface ports:

- calls `make_XH_obj_`*XHname* to set up the dynamics, including
  parameters

- calls
  [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)
  to set initial values

- initializes
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md):
  all weights are set to \\(1\\)

- initializes
  [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md)
  to the residence matrix: all time spent at home

- calls
  [setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md)
  with no events

- calls
  [setup_demog_obj](https://dd-harp.github.io/ramp.xds/reference/setup_demog_obj.md)
  setting birth and death rates to zero

- calls
  [setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md)
  with no time spent away

- calls
  [setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md)
  with the travel EIR set to zero

The function is part of basic setup, but it is also used to add a host
species.

## Usage

``` r
setup_XH_obj(XHname, residence, xds_obj, options = list(), i = 1)
```

## Arguments

- XHname:

  the model name

- residence:

  the residence vector

- xds_obj:

  an **`xds`** model object

- options:

  model options as a named list

- i:

  the host species index

## Value

an **`xds`** object

## Note

The model object is stored as `xds_obj$XH_obj[[i]]`

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md),
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md),
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md),
[setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md),
[setup_demog_obj](https://dd-harp.github.io/ramp.xds/reference/setup_demog_obj.md),
[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md)
