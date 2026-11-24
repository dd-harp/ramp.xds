# Glossary for **`ramp.xds`**

A vocabulary guide to model structure, mosquito behavior, exposure,
transmission, and the modular framework. Entries link to the concept
pages and functions that provide fuller explanations.

## Model Structure

- Patch:

  A spatial unit in which adult mosquito populations and interactions
  with resources are represented. See
  [xds_info_patch_dynamics](https://dd-harp.github.io/ramp.xds/reference/xds_info_patch_dynamics.md).

- Population stratum:

  A subdivision of a human/host population represented by its own
  population and infection variables. See
  [xds_info_strata](https://dd-harp.github.io/ramp.xds/reference/xds_info_strata.md).

- Aquatic habitat:

  A location within a patch where immature mosquitoes develop. See
  [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md).

- Residence:

  The patch where a population stratum lives, which need not be the only
  patch where it spends time. See
  [residence_vector](https://dd-harp.github.io/ramp.xds/reference/residence_vector.md).

- Residence matrix:

  A matrix that aggregates quantities in population strata by patch of
  residence. See
  [residence_matrix](https://dd-harp.github.io/ramp.xds/reference/residence_matrix.md).

- Habitat Membership:

  The patch location assigned to each aquatic habitat. See
  [habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md).

- Habitat matrix:

  A matrix that aggregates quantities in aquatic habitats by patch. See
  [habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/habitat_matrix.md).

## Mosquito Behavior and Resources

- Search weight:

  A weight assigned to a host stratum or habitat that contributes to its
  availability to searching mosquitoes. See
  [xds_info_search_weights](https://dd-harp.github.io/ramp.xds/reference/xds_info_search_weights.md).

- Availability:

  A measure of accessibility to mosquitoes, rather than abundance alone,
  that incorporates search weights and, for hosts, time at risk. See
  [available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md)
  and
  [habitat_availability](https://dd-harp.github.io/ramp.xds/reference/habitat_availability.md).

- Blood feeding:

  The interaction between mosquitoes searching for blood hosts and hosts
  spending time in patches. See
  [blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md).

- Egg laying:

  The allocation of eggs laid by adult mosquitoes in patches among
  aquatic habitats. See
  [egg_laying](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md).

- Laying matrix:

  A matrix that distributes eggs from patches to aquatic habitats
  according to relative availability. See
  [laying_matrix](https://dd-harp.github.io/ramp.xds/reference/laying_matrix.md).

- Emergence:

  The transition from aquatic immature stages to adult mosquitoes in
  patches. See
  [xds_info_emergence](https://dd-harp.github.io/ramp.xds/reference/xds_info_emergence.md).

- Bionomics:

  The demographic and behavioral properties of mosquitoes, including
  feeding, survival, movement, and development. See
  [xds_info_mosquito_bionomics](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_bionomics.md).

- Functional response:

  A relationship that expresses a mosquito bionomic parameter as a
  function of resource availability. See
  [blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md).

## Movement and Exposure

- Time spent:

  The fraction of time a population stratum spends in each patch when
  not traveling. See
  [xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md).

- Time at risk:

  Time spent when not traveling, weighted by mosquito activity. See
  [time_at_risk](https://dd-harp.github.io/ramp.xds/reference/time_at_risk.md).

- Time away:

  The fraction of time spent outside the spatial domain represented by
  the patches. See
  [xds_port_time_away](https://dd-harp.github.io/ramp.xds/reference/xds_port_time_away.md).

- Travel malaria:

  Exposure to malaria outside the modeled spatial domain. See
  [xds_info_travel_malaria](https://dd-harp.github.io/ramp.xds/reference/xds_info_travel_malaria.md).

- Visitor:

  A non-resident host whose availability and infectiousness contribute
  to blood feeding and transmission in patches. See
  [xds_info_visitors](https://dd-harp.github.io/ramp.xds/reference/xds_info_visitors.md).

- Local fraction:

  The fraction of blood feeding on local hosts among blood meals on
  local hosts and visitors. See
  [xds_info_local_fraction](https://dd-harp.github.io/ramp.xds/reference/xds_info_local_fraction.md).

- Environmental heterogeneity:

  Variation in exposure within a population stratum, represented through
  a model for the distribution of bites. See
  [xds_info_environmental_heterogeneity](https://dd-harp.github.io/ramp.xds/reference/xds_info_environmental_heterogeneity.md).

- Relative biting rate:

  A stratum's daily EIR relative to the population average daily EIR.
  See
  [xds_info_relative_biting_rates](https://dd-harp.github.io/ramp.xds/reference/xds_info_relative_biting_rates.md).

## Infection and Transmission

- Mixing matrix:

  The biting distribution matrix that maps biting density in patches to
  per-capita biting rates in population strata. See
  [mixing_matrix](https://dd-harp.github.io/ramp.xds/reference/mixing_matrix.md).

- HBR:

  Human/host biting rate: the number of mosquito bites per person or
  host per day. See
  [F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md).

- EIR:

  Entomological inoculation rate: the number of infective bites per
  person or host per day. See
  [F_eir](https://dd-harp.github.io/ramp.xds/reference/F_eir.md) and
  [xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md).

- FoI:

  Force of infection: the rate of acquiring infection. See
  [F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md) and
  [xds_info_exposure](https://dd-harp.github.io/ramp.xds/reference/xds_info_exposure.md).

- Attack rate:

  The probability of acquiring infection over a time step. See
  [F_ar](https://dd-harp.github.io/ramp.xds/reference/F_ar.md) and
  [Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md).

- Infected mosquito:

  A mosquito carrying infection, which need not yet be capable of
  transmitting it. See
  [MY_module_list](https://dd-harp.github.io/ramp.xds/reference/MY_module_list.md).

- Infective mosquito:

  A mosquito capable of transmitting the pathogen through a bite. See
  [F_fqZ](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.md).

- Infectivity:

  The probability of host infection per infective bite, potentially
  affected by immunity. See
  [F_infectivity](https://dd-harp.github.io/ramp.xds/reference/F_infectivity.md).

- Infectiousness:

  The capacity of an infected host population to infect mosquitoes
  during blood feeding. See
  [F_I](https://dd-harp.github.io/ramp.xds/reference/F_I.md).

- Net infectiousness:

  The probability that a mosquito becomes infected after feeding on the
  modeled hosts and visitors in a patch. See
  [F_kappa](https://dd-harp.github.io/ramp.xds/reference/F_kappa.md) and
  [xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md).

## Framework Terminology

- Component:

  A group of related dynamical processes: **XH** for host demography and
  infection, **MY** for adult mosquito ecology and infection, or **L**
  for immature mosquito ecology. See
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md).

- Module:

  An implementation of a dynamical component, selected during setup. See
  [xds_info_dynamical_components](https://dd-harp.github.io/ramp.xds/reference/xds_info_dynamical_components.md).

- Interface:

  A standardized set of computations that connects dynamical processes.
  See
  [xds_info_interfaces](https://dd-harp.github.io/ramp.xds/reference/xds_info_interfaces.md).

- Port:

  A parameter or variable whose value is set through a standardized
  function call with a flexible implementation. See
  [xds_info_port](https://dd-harp.github.io/ramp.xds/reference/xds_info_port.md).

- Junction:

  A function that coordinates a bundle of ports. See
  [xds_info_junction](https://dd-harp.github.io/ramp.xds/reference/xds_info_junction.md).

- Forcing:

  Externally specified variation in parameters or dynamical terms,
  implemented through ports, junctions, or trivial modules. See
  [xds_info_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_forcing.md)
  and
  [xds_info_trivial_forcing](https://dd-harp.github.io/ramp.xds/reference/xds_info_trivial_forcing.md).

## See also

[xds_help_info](https://dd-harp.github.io/ramp.xds/reference/xds_help_info.md),
[xds_help_guide](https://dd-harp.github.io/ramp.xds/reference/xds_help_guide.md)
