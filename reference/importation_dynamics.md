# Importation terms

Compute terms associated with importation

## Usage

``` r
importation_dynamics(t, xds_obj)
```

## Arguments

- t:

  time

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

## Importation

For each host species, this function calls four ports at time `t`:

- [update_visitors](https://dd-harp.github.io/ramp.xds/reference/update_visitors.md)
  updates the availability of non-resident hosts.

- [update_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/update_vis_kappa.md)
  updates the net infectiousness of those visitors.

- [update_time_away](https://dd-harp.github.io/ramp.xds/reference/update_time_away.md)
  updates the fraction of time resident hosts spend away.

- [update_travel_eir](https://dd-harp.github.io/ramp.xds/reference/update_travel_eir.md)
  updates their EIR while traveling.

Each port returns an updated `xds_obj`, which is passed to the next
port. Static ports leave the model unchanged. These inputs are used by
the blood-feeding and transmission interfaces to account for visitors
and by
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md) to
account for infection risk while traveling. This function updates the
inputs; it does not itself compute new infections or change the
dependent variables.

## See also

[malaria_importation](https://dd-harp.github.io/ramp.xds/reference/malaria_importation.md),
[time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
[visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md),
[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md),
[setup_visitors](https://dd-harp.github.io/ramp.xds/reference/setup_visitors.md),
[setup_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/setup_vis_kappa.md),
[setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
[setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md),
[get_visitors](https://dd-harp.github.io/ramp.xds/reference/get_visitors.md),
[change_visitors](https://dd-harp.github.io/ramp.xds/reference/change_visitors.md),
[get_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/get_vis_kappa.md),
[change_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/change_vis_kappa.md),
[get_time_away](https://dd-harp.github.io/ramp.xds/reference/get_time_away.md),
[change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md),
[get_travel_eir](https://dd-harp.github.io/ramp.xds/reference/get_travel_eir.md),
[change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_EIR.md)
