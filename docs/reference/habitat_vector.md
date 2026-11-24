# The habitat vector

Each aquatic habitat is located in a patch.

The habitat vector is passed to a basic setup function through the
argument `membership`.

- `membership` is a vector of positive integers

- `nHabitats <- length(membership)`

## Note

Information about patch locations is stored in the *habitat vector.*

## Habitat Vector

A habitat vector \\((V)\\) indexes the patches where each aquatic
habitat can be found.

If the \\(i^{th}\\) habitat is located in the \\(j^{th}\\) patch, then
\\(V_i = j.\\)

## See also

[xds_basic_setup](https://dd-harp.github.io/ramp.xds/reference/xds_basic_setup.md),
[habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/habitat_matrix.md)
