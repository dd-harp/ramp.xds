# Check K Matrix

Check that

- \\K\\ is a \\N_p \times N_p\\ matrix

- if not zero, the diagonal elements are all \\-1\\

- the columns sum to 0: tolerance is set by `tol`

## Usage

``` r
check_K_matrix(K, Np, tol = 1e-12)
```

## Arguments

- K:

  a mosquito dispersal matrix

- Np:

  the number of patches

- tol:

  tolerance

## See also

[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
