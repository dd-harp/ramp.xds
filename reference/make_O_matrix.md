# Compute the Laying Matrix

The egg laying matrix, \\O\\, allocates a portion of eggs laid by adult
mosquitoes in a patch to each one of the aquatic habitats in the patch

## Usage

``` r
make_O_matrix(search_weights, habitat_matrix, Q)
```

## Arguments

- search_weights:

  the habitat search weights

- habitat_matrix:

  the habitat membership matrix

- Q:

  the availability of egg-laying habitats

## Value

a `nHabitats` \\\times\\ `nPatches`
[matrix](https://rdrr.io/r/base/matrix.html) describing egg
distribution, \\O\\

## Details

The algorithm is motivated by the notion of mosquito *searching* for
aquatic habitat and a quantity we call *habitat availability*, \\Q\\.
Each habitat is assigned a *search weight,* \\\omega\\. In the simplest
case, habitat availability is the sum of \\\omega\\ by patch, which uses
the habitat membership matrix, \\N\\. If the habitats in the model got
all the eggs, then \$\$Q = N \cdot \omega.\$\$

The fraction of eggs in the \\j^{th}\\ patch laid in the \\i^{th}\\
habitat is \\\omega_i / Q_j.\\

To make the model extensible, habitat availability sums all the places
where mosquitoes might lay eggs, including ovitraps and unsuitable
habitats. If every element in \\Q\\ were positive, the egg distribution
matrix would be: \$\${O} = \mbox{diag}\left(\omega \right) \cdot {N}^T
\cdot \mbox{diag} \left(\frac{1}{Q}\right).\$\$ To avoid dividing by
zero, the zero elements in \\Q\\ are set to an arbitrary positive value.

## See also

The membership matrix \\N\\ is computed by
[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)

Total habitat availability, \\\cal Q\\, is computed by
[F_Q](https://dd-harp.github.io/ramp.xds/reference/F_q.md)

The availability of ovitraps and bad habitats is setup in
[setup_ML_interface](https://dd-harp.github.io/ramp.xds/reference/setup_ML_interface.md)
