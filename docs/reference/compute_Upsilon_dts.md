# Make the mosquito demography matrix for spatial RM model in continuous time

Make the mosquito demography matrix for spatial RM model in continuous
time

## Usage

``` r
compute_Upsilon_dts(eip, Omega)
```

## Arguments

- eip:

  the extrinsic incubation period

- Omega:

  the demographic matrix

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nPatches`
by `nPatches`
