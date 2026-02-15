# Create the habitat membership matrix, \\N\\

The habitat membership matrix, \\N\\, holds information about the patch
location of each habitat. It is part of the egg-laying and emergence
interface, making it possible to compute egg laying from patches to
habitats; and emergence from habitats to patches.

## Usage

``` r
make_habitat_matrix(nPatches, membership)
```

## Arguments

- nPatches:

  the number of patches, \\n_p\\

- membership:

  a vector describing the patch index for each habitat

## Value

the habitat membership [matrix](https://rdrr.io/r/base/matrix.html),
denoted \\N\\ where \\\left\|N\right\|= n_p \times n_q\\

## Details

Information about the patch location of each habitat is passed as the
membership vector, an ordered list of patch locations. If the \\i^{th}\\
habitat is in the \\j^{th}\\ patch, then \\{N}\_{j,i}=1.\\ Otherwise,
\\{N}\_{j,i}=0.\\

Since \\N\\ is a matrix, it is readily used for computation. Let:

- \\n_q = \\ `nHabitats`, the number of habitats;

- \\n_p = \\ `nPatches`, the number of patches.

If \\w\\ is any vector describing a quantity in habitats (*i.e.*,
\\\left\|w\right\|= n_q\\), then \$\$W={N}\cdot w\$\$ is a vector that
has summed \\w\\ by patch, and \\\left\|W\right\|= n_p\\.

## See also

compute_habitat matrix is called by
[`make_xds_object_template()`](https://dd-harp.github.io/ramp.xds/reference/make_xds_object_template.md)
and
[`setup_ML_interface()`](https://dd-harp.github.io/ramp.xds/reference/setup_ML_interface.md)

see
[`view_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/view_habitat_matrix.md)

## Examples

``` r
make_habitat_matrix(3, c(1,1,2,2,2))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    0    0    0
#> [2,]    0    0    1    1    1
#> [3,]    0    0    0    0    0
```
