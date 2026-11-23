# Compute habitat availability

This function computes habitat availability.

## Usage

``` r
F_available_habitat(habitat_matrix, search_weights)
```

## Arguments

- habitat_matrix:

  the membership matrix, \\(N\\)

- search_weights:

  the habitat search weights, \\(w\\)

## Value

a [vector](https://rdrr.io/r/base/vector.html) describing habitat
availability, \\(Q\\), of length `nPatches`

## Details

The availability of the habitats that we have defined in the model,
denoted \\(Q\\), sums search weights, \\(w\\), by patch using the
habitat membership matrix, \\(N\\), and we can compute \$\$Q = {N} \cdot
w.\$\$

## See also

This function is called by
[update_all_available_water](https://dd-harp.github.io/ramp.xds/reference/update_all_available_water.md)

[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
discusses \\(N\\)

The availability of ovitraps and bad habitats is set up in
[setup_egg_laying_interface](https://dd-harp.github.io/ramp.xds/reference/setup_egg_laying_interface.md)
