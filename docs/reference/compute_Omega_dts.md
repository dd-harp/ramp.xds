# Make the mosquito demography matrix for spatial RM model in discrete time

Make the mosquito demography matrix for spatial RM model in discrete
time

## Usage

``` r
compute_Omega_dts(p, ssigma, mu, calK)
```

## Arguments

- p:

  mosquito daily survival, a vector of length `nPatches`

- ssigma:

  mosquito daily fraction emigrating

- mu:

  emigration survival, a vector of length `nPatches`

- calK:

  a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions
  `nPatches` by `nPatches`

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nPatches`
by `nPatches`
