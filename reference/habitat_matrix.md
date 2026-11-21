# The habitat matrix

- \\\mathbf N\\:

  the habitat matrix

## Habitat Matrix

The habitat matrix \\(\mathbf N)\\ is an `nPatches`\\\times\\`nHabitats`
matrix that is constructed from the
[habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md).

If the \\i^{th}\\ habitat is in the \\j^{th}\\ patch, then
\\{N}\_{j,i}=1.\\ Otherwise, \\{N}\_{j,i}=0.\\

If \\x\\ is any vector describing a quantity in habitats (*i.e.*,
\\\left\|x\right\|=\\`nHabitats`), then \$\$X=\mathbf{N}\cdot x\$\$ is a
vector that has summed \\x\\ by patch, and
\\\left\|X\right\|=\\`nPatches`.

## See also

[habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md),
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md),
[habitat_availability](https://dd-harp.github.io/ramp.xds/reference/habitat_availability.md)
