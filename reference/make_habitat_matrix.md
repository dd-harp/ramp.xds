# Make the habitat matrix

Make the habitat matrix \\(\mathbf{N})\\ from the habitat vector.

## Usage

``` r
make_habitat_matrix(nPatches, membership)
```

## Arguments

- nPatches:

  the number of patches

- membership:

  a habitat vector (see
  [habitat_vector](https://dd-harp.github.io/ramp.xds/reference/habitat_vector.md))

## Value

the habitat matrix

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

## See also

[`get_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/get_habitat_matrix.md)

## Examples

``` r
make_habitat_matrix(3, c(1,1,2,2,2))
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    1    1    0    0    0
#> [2,]    0    0    1    1    1
#> [3,]    0    0    0    0    0
```
