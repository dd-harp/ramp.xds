# Compute Habitat Availability

Compute the availability of aquatic habitats.

## Usage

``` r
F_Q(habitat_matrix, search_weights)
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
[compute_Qall](https://dd-harp.github.io/ramp.xds/reference/compute_Qall.md)

[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
discusses \\N\\

The availability of ovitraps and bad habitats is setup in
[setup_ML_interface](https://dd-harp.github.io/ramp.xds/reference/setup_ML_interface.md)
