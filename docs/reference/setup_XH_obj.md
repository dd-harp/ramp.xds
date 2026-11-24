# Set up the module object (**XH**)

Set up the selected module and its interface ports for the \\(i^{th}\\)
host species in the **XH** component.

- Pass `residence`, `xds_obj`, `options`, and `i` to the selected setup
  method.

- Let that method construct the module object and configure its initial
  values, exposure, travel, demography, and treatment interfaces as
  appropriate for the module.

- Return the updated **`xds`** model object.

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

an **`xds`** model object

## Note

The module object is stored as `xds_obj$XH_obj[[i]]`. Dispatch is on
`XHname`, whose class is set to the supplied module name, not on an
existing module object.

## See also

[xds_setup](https://dd-harp.github.io/ramp.xds/reference/xds_setup.md),
[change_XH_pars](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.md),
[setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md),
[setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md),
[setup_mass_treatment](https://dd-harp.github.io/ramp.xds/reference/setup_mass_treatment.md),
[setup_demog_obj](https://dd-harp.github.io/ramp.xds/reference/setup_demog_obj.md),
[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md)
