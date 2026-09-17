# F_Omega for xde

Computes the mosquito demographic matrix: \$\$ \Omega = \mbox{diag}
\left( g + \sigma \mu \right) - K \cdot \mbox{diag} \left( \sigma
\left(1-\mu\right) \right) \$\$

## Usage

``` r
F_Omega_xde(g, sigma, mu, K_matrix)
```

## Arguments

- g:

  mosquito death rate, a vector of length `nPatches`

- sigma:

  mosquito emigration rate, a vector of length `nPatches`

- mu:

  emigration loss, a vector of length `nPatches`

- K_matrix:

  a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions
  `nPatches` by `nPatches`

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nPatches`
by `nPatches`

## See also

[xds_info_mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
