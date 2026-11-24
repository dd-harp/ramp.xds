# Compute beta, the biting distribution matrix

Compute beta, the biting distribution matrix

## Usage

``` r
F_beta(W, wts, TaR)
```

## Arguments

- W:

  human / host availability in the patches

- wts:

  the blood feeding search weights, adjusted for the fraction of time
  not traveling

- TaR:

  (time at risk), a [matrix](https://rdrr.io/r/base/matrix.html) with
  dimensions `nPatches` by `nStrata`

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nStrata`
by `nPatches`

## Mixing Matrix

The mixing matrix maps mosquito biting density in patches to per-capita
biting rates in human/host population strata. It uses the same search
weights and time at risk as
[available_blood_hosts](https://dd-harp.github.io/ramp.xds/reference/available_blood_hosts.md).

For a host species, the mixing matrix is an `nStrata` \\\times\\
`nPatches` matrix: \$\$\beta = \mbox{diag}(w(1-\delta)) \cdot \Psi^T
\cdot \mbox{diag}(1/W)\$\$ The product \\w(1-\delta)\\ is element-wise,
with \\\delta\\ denoting the fraction of time away for each stratum.
Here, \\W\\ is total local host availability, summed over host species.
F_beta replaces zero entries of \\W\\ with one before dividing, to avoid
division by zero.

For one host species, the fraction of bites on local hosts in patch
\\j\\ received by stratum \\i\\ is \\H_i \beta\_{i,j}\\. The entries of
\\\beta\\ alone are per-capita allocation weights, not fractions of
bites.

If \\D\\ is mosquito biting density in each patch and \\\ell\\ is the
local fraction, then the biting rate in each stratum is: \$\$\beta \cdot
(\ell D)\$\$ The product \\\ell D\\ is element-wise. Using infective
biting density gives the daily local EIR.
