# Make the laying matrix

Make the egg laying matrix \\((\mathbf {U})\\).

## Usage

``` r
make_U_matrix(search_weights, habitat_matrix, Q)
```

## Arguments

- search_weights:

  the habitat search weights

- habitat_matrix:

  the habitat membership matrix

- Q:

  the availability of egg-laying habitats

## Value

an `nHabitats` \\(\times\\) `nPatches`
[matrix](https://rdrr.io/r/base/matrix.html) describing egg
distribution, \\(U\\)

## Habitat Matrix

The habitat matrix \\((\mathbf N)\\) is an
`nPatches`\\(\times\\)`nHabitats` matrix that is constructed from the
[habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md).

If the \\(i^{th}\\) habitat is in the \\(j^{th}\\) patch, then
\\({N}\_{j,i}=1.\\) Otherwise, \\({N}\_{j,i}=0.\\)

If \\(x\\) is any vector describing a quantity in habitats (*i.e.*,
\\(\left\|x\right\|=\\)`nHabitats`), then \$\$X=\mathbf{N}\cdot x\$\$ is
a vector that has summed \\(x\\) by patch, and
\\(\left\|X\right\|=\\)`nPatches`.

## Habitat Availability

Egg laying by adult mosquitoes was modeled as the outcome of *searching*
for aquatic habitats.

Each habitat is assigned a *search weight* \\((w).\\)

Habitat availability in a patch is the sum of the search weights of all
the habitats in that patch.

Habitat availability \\((Q)\\) is computed using the habitat matrix
\\((\mathbf{N})\\):

\$\$Q = \mathbf{N}\cdot w\$\$

- Patch emigration rates and egg laying rates by adult mosquitoes can be
  modeled as a functional response to habitat availability.

## Laying Matrix

The egg laying matrix \\((\mathbf {U})\\) distributes the eggs laid by
the adult mosquitoes in each patch among the aquatic habitats in that
patch by relative availability.

The algorithm is motivated by *searching* for aquatic habitat. Each
habitat is assigned a *search weight* \\((w).\\) Habitat availability
\\((Q)\\) sums the search weights by patch. The fraction of all eggs
laid in the \\(j^{th}\\) patch that end up in the \\(i^{th}\\) habitat
is \\(w_i / Q_j.\\)

This is computed by constructing the laying matrix, \\(\mathbf U\\), a
`nHabitats` \\(\times\\) `nPatches` matrix. To avoid division-by-zero
errors for patches with no available habitats, a quantity \\(\epsilon =
1e-14\\) is added to availability: \$\$ \mathbf{U}= \mbox{diag}\left( w
\right) \cdot \mathbf{N}^T \cdot \mbox{diag} \left(
\frac{1}{\textstyle{Q + \epsilon}} \right) \$\$

## See also

The membership matrix \\(N\\) is computed by
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)

Total habitat availability, \\(Q\\), is computed by
[F_available_habitat](https://dd-harp.github.io/ramp.xds/reference/F_available_habitat.md)

The availability of ovitraps and bad habitats is set up in
[setup_egg_laying_interface](https://dd-harp.github.io/ramp.xds/reference/setup_egg_laying_interface.md)
