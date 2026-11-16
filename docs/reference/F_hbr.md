# Compute the daily Human Biting Rate (HBR)

Compute the daily HBR for each human population stratum from the
mosquito biting density in each patch, the fraction of bites on
residents, and the biting distribution matrix. An empty `fqM` yields
zero HBR, as for a MY module without an adult mosquito population.

## Usage

``` r
F_hbr(fqM, beta, local_frac)
```

## Arguments

- fqM:

  mosquito biting density on humans in each patch

- beta:

  biting distribution matrix, mapping patches to population strata

- local_frac:

  fraction of bites in each patch that occur on residents

## Value

[numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
