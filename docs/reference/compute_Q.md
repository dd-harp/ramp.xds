# Compute the total availability of egg-laying habitats, \\Q\\

Compute the availability of aquatic habitats.

## Usage

``` r
compute_Q(habitat_matrix, search_weights)
```

## Arguments

- habitat_matrix:

  the membership matrix, \\N\\

- search_weights:

  the habitat search weights, \\\omega_q\\

## Value

a [vector](https://rdrr.io/r/base/vector.html) of describing habitat
availability, \\Q\\, of length `nPatches`

## Details

The availability of the habitats that we have defined in the model,
denoted \\Q\\, sums search weights, \\\omega_q\\, by patch using the
habitat membership matrix, \\N\\, and we can compute \$\$Q = {N} \cdot
\omega_q.\$\$

## See also

This function is called by
[make_Q](https://dd-harp.github.io/ramp.xds/reference/make_Q.md)

[create_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/create_habitat_matrix.md)
discusses \\N\\

The availability of ovitraps and bad habitats is setup in
[setup_EGG_LAYING](https://dd-harp.github.io/ramp.xds/reference/setup_EGG_LAYING.md)
