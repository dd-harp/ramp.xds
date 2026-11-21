# Compute Available Laying Sites, \\Q\_{all}\\

Mosquitoes lay eggs in aquatic habitats, in water bodies that are
unsuitable for larval development (bad habitats), and in ovitraps.

## Usage

``` r
F_all_available_water(Q, Q_traps, Q_bad)
```

## Arguments

- Q:

  the availability of habitats

- Q_traps:

  the availability of traps

- Q_bad:

  the availability of unsuitable habitats

## Value

Availability of laying sites, \\Q\_{all}\\

## Details

Habitat availability denoted \\Q\\, sums habitat search weights, \\w\\,
by patch using the habitat membership matrix, \\N\\: \$\$Q = {N} \cdot
w.\$\$

If some ovitraps (\\Q_t\\) and bad_habitats (\\Q_b\\) are also
available, then total availability of laying sites is \$\$Q\_{all} = Q +
Q_t + Q_b.\$\$

Habitat availability is computed elsewhere and stored in
`xds_obj$terms$Q`, ovitrap availability in `xds_obj$patches$ovitraps`,
and bad-habitat availability in `xds_obj$patches$bad_habitats`.

## See also

This function is called by
[update_all_available_water](https://dd-harp.github.io/ramp.xds/reference/update_all_available_water.md)

[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
discusses \\N\\

The availability of traps and bad habitats is set up in
[setup_egg_laying_interface](https://dd-harp.github.io/ramp.xds/reference/setup_egg_laying_interface.md)
