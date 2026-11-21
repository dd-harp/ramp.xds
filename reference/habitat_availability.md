# Available habitats

- \\w\\:

  habitat search weights

- \\Q\\:

  habitat availability

## Note

- \\\left\|w\right\| =\\`nHabitats`

- \\\left\|Q\right\| =\\`nPatches`

## Habitat Availability

Egg laying by adult mosquitoes was modeled as the outcome of *searching*
for aquatic habitats.

Each habitat is assigned a *search weight* \\(w).\\

Habitat availability in a patch is the sum of the search weights of all
the habitats in that patch.

Habitat availability \\(Q)\\ is computed using the habitat matrix
\\(\mathbf{N})\\:

\$\$Q = \mathbf{N}\cdot w\$\$

- Patch emigration rates and egg laying rates by adult mosquitoes can be
  modeled as a functional response to habitat availability.

## See also

[habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/habitat_matrix.md),
[laying_matrix](https://dd-harp.github.io/ramp.xds/reference/laying_matrix.md)
