# F_Omega for dts

F_Omega for dts

## Usage

``` r
F_Omega_dts(p, ssigma, mu, K_matrix)
```

## Arguments

- p:

  mosquito daily survival, a vector of length `nPatches`

- ssigma:

  mosquito daily fraction emigrating

- mu:

  emigration survival, a vector of length `nPatches`

- K_matrix:

  a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions
  `nPatches` by `nPatches`

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nPatches`
by `nPatches`
