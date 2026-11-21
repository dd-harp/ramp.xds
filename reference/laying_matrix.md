# The laying matrix

- \\\mathbf{U}\\:

  The laying matrix

## Laying Matrix

The egg laying matrix \\(\mathbf {U})\\ distributes the eggs laid by the
adult mosquitoes in each patch among the aquatic habitats in that patch
by relative availability.

The algorithm is motivated by *searching* for aquatic habitat. Each
habitat is assigned a *search weight* \\(w).\\ Habitat availability
\\(Q)\\ sums the search weights by patch. The fraction of all eggs laid
in the \\j^{th}\\ patch that end up in the \\i^{th}\\ habitat is \\w_i /
Q_j.\\

This is computed by constructing the laying matrix, \\\mathbf U\\, a
`nHabitats` \\\times\\ `nPatches` matrix. To avoid division-by-zero
errors for patches with no available habitats, a quantity \\\epsilon =
1e-14\\ is added to availability: \$\$ \mathbf{U}= \mbox{diag}\left( w
\right) \cdot \mathbf{N}^T \cdot \mbox{diag} \left(
\frac{1}{\textstyle{Q + \epsilon}} \right) \$\$

## See also

[habitat_availability](https://dd-harp.github.io/ramp.xds/reference/habitat_availability.md)
