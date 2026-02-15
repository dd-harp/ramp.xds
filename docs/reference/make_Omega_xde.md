# Make the mosquito demography matrix for spatial RM model in continuous time

Make the mosquito demography matrix for spatial RM model in continuous
time

## Usage

``` r
make_Omega_xde(g, sigma, mu, K_matrix)
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
