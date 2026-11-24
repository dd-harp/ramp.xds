# Set up the module object for `SIS` (**XH**)

Set up a `SIS` module object for the **XH** component and the \\i^{th}\\
host species.

- Set the module name to `SIS`.

- Create the module object with
  [make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md),
  passing `nStrata[i]` and `options`.

- Set initial values using
  [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md),
  passing `options` to override defaults.

- Initialize blood search weights using
  [setup_blood_search_weights.setup](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.setup.md).

- Initialize the time-spent matrix using
  [setup_timespent.setup](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.setup.md).

- Initialize mass-treatment functions using
  [setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md).

- Initialize birth and death rates using
  [setup_demog_obj](https://dd-harp.github.io/ramp.xds/reference/setup_demog_obj.md).

- Initialize time away using
  [setup_time_away.setup](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.setup.md).

- Initialize travel EIR using
  [setup_travel_eir.no_travel](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.no_travel.md).

- Return the updated **`xds`** model object.

## Usage

``` r
# S3 method for class 'SIS'
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

an **`xds`** model object

## Note

The module object is stored at `xds_obj$XH_obj[[i]]`

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## Parameters

The parameters that are returned by
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and also modifiable by
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)
describe the dynamics or test positivity.

- `b`:

  the fraction of infective bites that would cause an infection

- `r`:

  clearance rate for infections

- `c`:

  the probability an infected person would infect a mosquito

- `rho` or \\\rho\\:

  the fraction of incident infections that are treated and cured

- `sigma` or \\\sigma\\:

  the rate that prevalent infections are treated and cured

- `q`:

  test positivity

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md),
[make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md)
