# Malaria Importation

Malaria can be imported from outside an area when residents acquire
infections while traveling or when infected visitors spend time in the
area, get bitten by mosquitoes, and infect them.

For the travel pathway, see
[travel_malaria](https://dd-harp.github.io/ramp.xds/reference/travel_malaria.md).
Importation through infectious visitors depends on
[visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md)
and
[visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md).

The importation junction represents these processes using four terms:

- [time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md):

  the fraction of time residents spend outside the area

- [travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md):

  the EIR residents experience while traveling

- [visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md):

  the availability of non-resident humans to mosquitoes

- [visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md):

  the probability a mosquito becomes infected after feeding on a visitor

These parameters and variables are fully integrated into the interfaces
that compute blood feeding, transmission, and exposure. By default, they
are set to have no effect, but they can be configured using setup
functions.

## Note

[Importation](https://dd-harp.github.io/ramp.xds/reference/Importation.md)
is called by
[xds_compute_terms](https://dd-harp.github.io/ramp.xds/reference/xds_compute_terms.md).

There are no terms to model importation by immigrating infected
mosquitoes.

## See also

- **Terms** —
  [time_away](https://dd-harp.github.io/ramp.xds/reference/time_away.md),
  [travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md),
  [visitor_availability](https://dd-harp.github.io/ramp.xds/reference/visitor_availability.md),
  [visitor_infectiousness](https://dd-harp.github.io/ramp.xds/reference/visitor_infectiousness.md)

- **Setup** —
  [setup_visitors](https://dd-harp.github.io/ramp.xds/reference/setup_visitors.md),
  [setup_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/setup_vis_kappa.md),
  [setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md),
  [setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md)

- **Get and change** —
  [get_visitors](https://dd-harp.github.io/ramp.xds/reference/get_visitors.md),
  [change_visitors](https://dd-harp.github.io/ramp.xds/reference/change_visitors.md),
  [get_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/get_vis_kappa.md),
  [change_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/change_vis_kappa.md),
  [get_time_away](https://dd-harp.github.io/ramp.xds/reference/get_time_away.md),
  [change_time_away](https://dd-harp.github.io/ramp.xds/reference/change_time_away.md),
  [get_travel_eir](https://dd-harp.github.io/ramp.xds/reference/get_travel_eir.md),
  [change_travel_eir](https://dd-harp.github.io/ramp.xds/reference/change_travel_eir.md)

- **Computation** —
  [update_visitors](https://dd-harp.github.io/ramp.xds/reference/update_visitors.md),
  [update_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/update_vis_kappa.md),
  [update_time_away](https://dd-harp.github.io/ramp.xds/reference/update_time_away.md),
  [update_travel_eir](https://dd-harp.github.io/ramp.xds/reference/update_travel_eir.md),
  [importation_dynamics](https://dd-harp.github.io/ramp.xds/reference/importation_dynamics.md)
