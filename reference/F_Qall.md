# Compute Available Laying Sites, \\O\\

Mosquitoes lay eggs in aquatic habitats, in water bodies that are
unsuitable for larval development (bad habitats), and in ovitraps.

## Usage

``` r
F_Qall(Q, Q_traps, Q_bad)
```

## Arguments

- Q:

  the availability of habitats

- Q_traps:

  the availability of traps

- Q_bad:

  the availability of unsuitable habitats

## Value

Availability laying sites, \\O\\

## Details

Habitat availability denoted \\Q\\, sums habitat search weights,
\\\omega\\, by patch using the habitat membership matrix, \\N\\: \$\$Q =
{N} \cdot \omega_h.\$\$

If some ovitraps (\\Q_t\\) and bad_habitats (\\Q_b\\) are also
available, then total availability of laying sites is \$\$O = Q + Q_t +
Q_b.\$\$

The availability of habitats, ovitraps and bad_habitats are computed
elsewhere and stored on `xds_obj$vars`.

## See also

This function is called by
[compute_Qall](https://dd-harp.github.io/ramp.xds/reference/compute_Qall.md)

[make_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/make_habitat_matrix.md)
discusses \\N\\

The availability of traps and bad habitats is setup in
[setup_ML_interface](https://dd-harp.github.io/ramp.xds/reference/setup_ML_interface.md)
