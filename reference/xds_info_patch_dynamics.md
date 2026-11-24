# Patch Dynamics

- `nPatches`:

  the number of patches

A spatial domain is sub-divided into a set of patches. The patches are
used to model adult mosquito ecology,
[xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md),
and
[xds_info_transmission](https://dd-harp.github.io/ramp.xds/reference/xds_info_transmission.md).

Each patch could contain some number of aquatic habitats (see
[xds_info_aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/xds_info_aquatic_habitats.md)).
Mosquitoes disperse among patches as they search for resources,
including blood hosts and aquatic habitats (see
[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)).

Patches are used to model blood feeding (see
[xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)),
so there is a corresponding human model for time spent (see
[xds_info_time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md)).
