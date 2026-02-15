# Compute the total availability of egg-laying habitats, \\Q\\

The sum of aquatic habitats and any other place a mosquito might lay
eggs, including ovitraps and unsuitable habitats.

## Usage

``` r
compute_Qtot(Q, Q_ovitraps, Q_bad_habitats)
```

## Arguments

- Q:

  the availability of ovitraps

- Q_ovitraps:

  the availability of ovitraps

- Q_bad_habitats:

  the availability of unsuitable habitats

## Value

a [vector](https://rdrr.io/r/base/vector.html) of describing habitat
availability, \\Q\\, of length `nPatches`

## Details

The availability of the habitats that we have defined in the model,
denoted \\Q\\, sums search weights, \\\omega_h\\, by patch using the
habitat membership matrix, \\N\\, and we can compute \$\$Q = {N} \cdot
\omega_h.\$\$ If some ovitraps and bad_habitats are also available, with
values \\Q_o\\ and \\Q_b\\ respectively, then \$\$Q = Q_h + Q_o +
Q_b.\$\$ The availability of habitats, ovitraps and bad_habitats are
computed elsewhere and stored on `pars$vars`.

## See also

This function is called by
[make_Q](https://dd-harp.github.io/ramp.xds/reference/make_Q.md)

[create_habitat_matrix](https://dd-harp.github.io/ramp.xds/reference/create_habitat_matrix.md)
discusses \\N\\

The availability of ovitraps and bad habitats is setup in
[setup_EGG_LAYING](https://dd-harp.github.io/ramp.xds/reference/setup_EGG_LAYING.md)
