# Patch Dynamics

Spatial dynamics in **`ramp.xds`** are patch-based, also known as
*metapopulation dynamics.* A *patch* is the primary spatial unit for
modeling transmission, and the number of patches is a core structural
parameter:

- `nPatches`:

  the number of patches

In patch-based models, a patch is a spatial unit that structures adult
mosquito populations. A patch could have any number of aquatic habitats
(see
[aquatic_habitats](https://dd-harp.github.io/ramp.xds/reference/aquatic_habitats.md)),
and mosquitoes disperse among patches as they search for blood hosts,
aquatic habitats to lay eggs, and other resources they need (see
[mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/mosquito_dispersal.md)).

Patches are used to model blood feeding (see
[blood_feeding](https://dd-harp.github.io/ramp.xds/reference/blood_feeding.md)),
so there is a corresponding human model for time spent (see
[time_spent](https://dd-harp.github.io/ramp.xds/reference/time_spent.md)).
