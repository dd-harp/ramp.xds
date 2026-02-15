# Compute beta, the biting distribution matrix

Compute beta, the biting distribution matrix

## Usage

``` r
F_beta(H, W, wts_f, TaR)
```

## Arguments

- H:

  human / host population density

- W:

  human / host availability in the patches

- wts_f:

  the blood feeding search weights

- TaR:

  (time at risk), a [matrix](https://rdrr.io/r/base/matrix.html)
  dimensions `nPatches` by `nStrata`

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nStrata`
by `nPatches`
