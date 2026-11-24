# The residence vector

Each human/host population stratum resides in a patch.

The residence vector is passed to a basic setup function through the
argument `residence`.

- `residence` is a vector of positive integers indexing patches

- `nStrata <- length(residence)` for a single host species

## Note

The residence vector is stored on the **XH** module object. Each host
species has its own residence vector.

## Residence Vector

The residence vector indexes the patch where each population stratum
resides. If the \\i^{th}\\ stratum resides in the \\j^{th}\\ patch, then
`residence[i] = j`.

Residence identifies where a stratum lives, not necessarily where it
spends all its time. Movement within the spatial domain is described by
the time spent matrix (see
[time_spent_matrix](https://dd-harp.github.io/ramp.xds/reference/time_spent_matrix.md)).

## See also

[xds_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md),
[get_residence](https://dd-harp.github.io/ramp.xds/reference/get_residence.md),
[residence_matrix](https://dd-harp.github.io/ramp.xds/reference/residence_matrix.md)
