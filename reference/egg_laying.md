# Egg Laying in **`ramp.xds`**

- \\\eta\\:

  The number of eggs laid in each habitat each day

- \\G\\:

  Number of eggs laid by adult mosquitoes in each patch, each day

- \\\zeta\\:

  Fraction of eggs laid in a suitable habitat

- \\\mathbf{U}\\:

  The laying matrix

## Egg Laying

**Egg Laying in Patches** — The number of eggs laid by adult mosquitoes
in each patch \\(G)\\ is computed by an **MY** component function
[F_eggs](https://dd-harp.github.io/ramp.xds/reference/F_eggs.md),
typically: \$\$G = F_G(M).\$\$

**Habitats and Traps** — Aquatic habitats compete with "bad habitats"
(*i.e.*, water bodies where eggs are laid that are not productive) and
ovitraps for eggs laid. The availability of each one in the patch is
given by:

- \\Q\\ — habitat availability (see
  [habitat_availability](https://dd-harp.github.io/ramp.xds/reference/habitat_availability.md))

- \\W\\ — availability of unsuitable habitats

- \\O\\ — availability of ovitraps

\$\$\zeta = \frac{\textstyle{Q}}{\textstyle{Q+W+O}}\$\$

**Laying in Habitats** — eggs laid in habitats are distributed by the
laying matrix \\(\mathbf{U})\\.

\$\$\eta = \mathbf{U} \cdot \zeta G \$\$

## See also

[F_eggs](https://dd-harp.github.io/ramp.xds/reference/F_eggs.md),
[habitat_availability](https://dd-harp.github.io/ramp.xds/reference/habitat_availability.md),
[laying_matrix](https://dd-harp.github.io/ramp.xds/reference/laying_matrix.md)
