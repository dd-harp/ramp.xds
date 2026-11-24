# Emergence in **`ramp.xds`**

Adult mosquitoes emerge from pupae in aquatic habitats. The process is
handled through the habitat interface.

## Emergence

During setup, the habitat matrix \\((\mathbf{N})\\) is built from the
habitat membership vector by
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md).
The function
[Emergence](https://dd-harp.github.io/ramp.xds/reference/Emergence.md)
calls
[F_emerge](https://dd-harp.github.io/ramp.xds/reference/F_emerge.md) to
get the adult mosquitoes emerging from each habitat \\((\alpha)\\) and
computes and stores emergence by patch (`Lambda` or \\(\Lambda\\)):
\$\$\Lambda = \mathbf{N} \cdot \alpha\$\$

## See also

[get_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/get_habitat_matrix.md),
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
