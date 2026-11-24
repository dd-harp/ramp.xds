# Aquatic Habitats

Aquatic habitats are distributed among patches on a landscape.

- `membership`:

  the [habitat
  vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md)

- `nHabitats`:

  the number of habitats (`length(membership)`)

- `search_weights` or \\w\\:

  a vector: a search weight for each aquatic habitat

## Habitat Vector

A habitat vector \\(V)\\ indexes the patches where each aquatic habitat
can be found.

If the \\i^{th}\\ habitat is located in the \\j^{th}\\ patch, then \\V_i
= j.\\

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

## Unproductive Aquatic Habitats

To model egg laying, a port was developed to model the availability of
unproductive aquatic habitats: water where mosquitoes lay eggs, but no
mature adults ever emerge.

## Ovitraps

To simulate ovitraps, a port was developed to model the availability of
the ovitraps.

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
