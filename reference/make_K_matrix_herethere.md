# Make a here-and-there dispersal matrix

Mosquitoes disperse to every other patch with equal probability.

## Usage

``` r
make_K_matrix_herethere(nPatches)
```

## Arguments

- nPatches:

  is the number of patches

## Value

a mosquito dispersal [matrix](https://rdrr.io/r/base/matrix.html) of
dimensions `nPatches` by `nPatches`

## See also

[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
