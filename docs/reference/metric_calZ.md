# Parasite Dispersal through one Parasite Generation (Mosquitoes)

Computes a `p` by `p` matrix describing parasite dispersal from
infecteds (columns) to infectees (rows). \$\$\mathcal{Z} =
e^{-\Omega\tau} \cdot \mbox{diag}\left( \frac{fq M}{W} \right) \cdot
{\cal D} \cdot fq\Omega^{-1}\$\$

## Usage

``` r
metric_calZ(Omega, tau, f, q, M, W, calD)
```

## Arguments

- Omega:

  the mosquito demography matrix

- tau:

  duration of the extrinsic incubation period

- f:

  the feeding rate

- q:

  fraction of bloodmeals taken on humans

- M:

  size of mosquito population in each patch

- W:

  ambient human population at each patch

- calD:

  parasite dispersal by humans matrix (see
  [metric_calD](https://dd-harp.github.io/ramp.xds/reference/metric_calD.md))

## Value

a numeric [matrix](https://rdrr.io/r/base/matrix.html)
