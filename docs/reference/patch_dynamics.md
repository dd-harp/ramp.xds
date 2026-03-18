# Patch Dynamics

- `nPatches`:

  the number of patches

A spatial domain is sub-divided into a set of patches. The patches are
used to model adult mosquito ecology,
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md),
and
[Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md).

Each patch could some number of aquatic habitats (see
[aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/aquatic_habitats.md)).
Mosquitoes disperse among patches as they search for or resources and
blood feed (if there are any blood hosts), lay eggs (if there are any
aquatic habitats), nd other resources they need (see
[mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/mosquito_dispersal.md)).

Patches are used to model blood feeding (see
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)),
so there is a corresponding human model for time spent (see
[time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent.md)).
