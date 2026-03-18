# Aquatic Habitats

Aquatic habitats are distributed among patches on a landscape.

- `membership`:

  a vector: the patch index of each aquatic habitat

- `nHabitats`:

  the number of habitats (`length(membership)`)

- `searchQ`:

  a vector: a search weight for each aquatic habitat

## The Habitat Matrix

The *habitat matrix* is an `nPatches` \\\times\\ `nHabitats` matrix
called \\N\\. It is constructed from the `membership` vector. In the
matrix, \\N\_{i,j}\\ is 1 if the \\i^{th}\\ patch contains the
\\j^{th}\\ habitat. For example, a membership vector \\(1,1,2)\\ has a
corresponding habitat matrix: \$\$ N = \left\[ \begin{array}{ccc}
1&1&0\\ 0&0&1\\ \end{array} \right\] \$\$ If \\\alpha\\ denotes the
emergence rate of adult mosquitoes from each habitat, then the emergence
rate at the patch, \\\Lambda\\ is: \$\$\Lambda = N \cdot \alpha\$\$

## The Egg Distribution Matrix

Let \\\omega\\ denote the habitat search weights. Habitat availability
(\\Q\\) is computed as: \$\$Q = N \cdot \omega.\$\$ Mosquitoes could
also lay some eggs in available bad habitats (\\B\\) or in available
ovitraps (\\O\\). The egg distribution matrix (\\U\\) is computed as
\$\$U = \left( \mbox{diag} \left(\omega \right) \cdot N^T \right) \cdot
\mbox{diag}\left( (Q+B+O)^{-1} \right).\$\$ In models where there are
patches with no available habitat, \\(Q+B+O)^{-1}\\ is computed using
the function
[diag_inverse](https://dd-harp.github.io/ramp.xds/reference/diag_inverse.md),
which fixes the divide-by-zero problem.
