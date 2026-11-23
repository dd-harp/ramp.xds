# Compute eggs laid in aquatic habitats

Compute the number of eggs laid at time \\(t\\) in each habitat.

## Usage

``` r
EggLaying(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object

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

## See also

[`setup_egg_laying_interface()`](https://dd-harp.github.io/ramp.xds/reference/setup_egg_laying_interface.md)
