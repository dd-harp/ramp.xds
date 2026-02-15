# Parasite Dispersal through one Parasite Generation (Humans)

Computes a `n` by `n` matrix describing parasite dispersal from
infecteds (columns) to infectees (rows). \$\$\mathcal{R} = b \beta \cdot
{\cal V} \cdot \mbox{diag}\left(W \right) \cdot \beta^T \cdot
\mbox{diag}\left(DH\right)\$\$

## Usage

``` r
metric_calR(b, beta, calV, W, D, H)
```

## Arguments

- b:

  transmission efficiency from mosquitoes to humans

- beta:

  the biting distribution matrix

- calV:

  parasite dispersal by mosquitoes matrix (see
  [metric_calV](https://dd-harp.github.io/ramp.xds/reference/metric_calV.md))

- W:

  ambient human population at each patch

- D:

  human transmitting capacity

- H:

  human population size of each strata

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
