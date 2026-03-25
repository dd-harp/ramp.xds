# Patch Dynamics

- `nPatches`:

  the number of patches

A spatial domain is sub-divided into a set of patches. The patches are
used to model adult mosquito ecology,
[xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md),
and
[xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md).

Each patch could some number of aquatic habitats (see
[xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)).
Mosquitoes disperse among patches as they search for or resources and
blood feed (if there are any blood hosts), lay eggs (if there are any
aquatic habitats), nd other resources they need (see
[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)).

Patches are used to model blood feeding (see
[xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md)),
so there is a corresponding human model for time spent (see
[xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/xds_info_time_spent.md)).
