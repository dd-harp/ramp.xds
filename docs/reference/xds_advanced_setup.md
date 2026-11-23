# Advanced setup for **`ramp.xds`**

Advanced setup options for progressive elaboration after basic setup
(see
[xds_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md))

## Spatial Dynamics

**Mosquito Dispersal**

- Dispersal Kernel —
  [setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)

- Patch Emigration —
  [setup_mosy_emigration](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  \|
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- **Blood** —
  [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md)
  \|
  [setup_other_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/setup_other_blood_hosts.md)
  \|
  [setup_blood_traps](https://dd-harp.github.io/ramp.xds/reference/setup_blood_traps.md)

- **Habitats** —
  [setup_habitat_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_habitat_search_weights.md)
  \|
  [setup_bad_habitats](https://dd-harp.github.io/ramp.xds/reference/setup_bad_habitats.md)
  \|
  [setup_ovitraps](https://dd-harp.github.io/ramp.xds/reference/setup_ovitraps.md)

- **Sugar** —
  [ramp.forcing::setup_sugar](https://dd-harp.github.io/ramp.forcing/reference/setup_sugar.html)

**Human Mobility**

- [setup_timespent](https://dd-harp.github.io/ramp.xds/reference/setup_timespent.md)

- [setup_F_circadian](https://dd-harp.github.io/ramp.xds/reference/setup_F_circadian.md)

## Human Demography

- [setup_births](https://dd-harp.github.io/ramp.xds/reference/setup_births.md)
  \|
  [setup_deaths](https://dd-harp.github.io/ramp.xds/reference/setup_deaths.md)

## Blood Feeding, Exposure, and Transmission

- [setup_blood_search_weights](https://dd-harp.github.io/ramp.xds/reference/setup_blood_search_weights.md)

- [setup_blood_feeding_rates](https://dd-harp.github.io/ramp.xds/reference/setup_f.md)
  \|
  [setup_human_fraction](https://dd-harp.github.io/ramp.xds/reference/setup_q.md)

- Resources:
  [setup_other_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/setup_other_blood_hosts.md)
  \|
  [setup_blood_traps](https://dd-harp.github.io/ramp.xds/reference/setup_blood_traps.md)

## Exposure

- [setup_exposure](https://dd-harp.github.io/ramp.xds/reference/setup_exposure.md)

- 

## Malaria Importation

- [setup_time_away](https://dd-harp.github.io/ramp.xds/reference/setup_time_away.md)
  \|
  [setup_travel_eir](https://dd-harp.github.io/ramp.xds/reference/setup_travel_eir.md)

- [setup_visitors](https://dd-harp.github.io/ramp.xds/reference/setup_visitors.md)
  \|
  [setup_vis_kappa](https://dd-harp.github.io/ramp.xds/reference/setup_vis_kappa.md)

## Mosquito Bionomics

- \\(f\\) —
  [setup_f](https://dd-harp.github.io/ramp.xds/reference/setup_f.md) \|
  [setup_blood_feeding_rates](https://dd-harp.github.io/ramp.xds/reference/setup_f.md)

- \\(q\\) —
  [setup_q](https://dd-harp.github.io/ramp.xds/reference/setup_q.md) \|
  [setup_human_fraction](https://dd-harp.github.io/ramp.xds/reference/setup_q.md)

- \\(g\\) —
  [setup_g](https://dd-harp.github.io/ramp.xds/reference/setup_g.md) \|
  [setup_mosquito_mortality](https://dd-harp.github.io/ramp.xds/reference/setup_g.md)

- \\(\sigma\\) —
  [setup_sigma](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)
  \|
  [setup_mosy_emigration](https://dd-harp.github.io/ramp.xds/reference/setup_sigma.md)

- \\(\mu\\) —
  [setup_mu](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)
  \|
  [setup_emigration_loss](https://dd-harp.github.io/ramp.xds/reference/setup_mu.md)

- \\(\nu\\) —
  [setup_nu](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)
  \|
  [setup_egg_batch_rates](https://dd-harp.github.io/ramp.xds/reference/setup_nu.md)

- \\(\tau\\) —
  [setup_eip](https://dd-harp.github.io/ramp.xds/reference/setup_eip.md)

## Weather

- [ramp.forcing::setup_temperature](https://dd-harp.github.io/ramp.forcing/reference/setup_temperature.html)

- 

## Resources

[xds_info_resources](https://dd-harp.github.io/ramp.xds/reference/xds_info_resources.md)

## Habitats and Egg Laying

- [xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)

- [xds_port_ovitraps](https://dd-harp.github.io/ramp.xds/reference/xds_port_ovitraps.md)
