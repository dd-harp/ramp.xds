# Compute the daily Entomological Inoculation Rate (EIR)

Compute the daily EIR for a set of human population strata given the
biting density of infective mosquitoes in each patch

## Usage

``` r
F_eir(fqZ, beta, local_frac)
```

## Arguments

- fqZ:

  the infective biting density

- beta:

  the mixing matrix

- local_frac:

  is the fraction of bites occurring on residents

## Value

[numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
