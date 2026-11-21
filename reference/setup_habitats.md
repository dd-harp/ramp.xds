# Set up the aquatic habitats

Set up a part of the `xds` object that defines the interface for egg
laying.

## Usage

``` r
setup_habitats(xds_obj, membership)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- membership:

  a vector of counting numbers

## Value

an **`xds`** model object

## Details

Store the habitat vector, set up the habitat matrix, and initialize the
laying matrix list.

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

The habitat membership matrix is created by
[`make_habitat_matrix()`](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
