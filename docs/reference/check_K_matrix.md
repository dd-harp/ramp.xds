# Check K Matrix

Check that

- \\M\\ is a \\N_p \times N_p\\ matrix

- if not zero, the diagonal elements are all \\-1\\

- the columns sum to 0: tolerance is set by `tol`

## Usage

``` r
check_K_matrix(M, Np, tol = 1e-12)
```

## Arguments

- M:

  a mosquito dispersal matrix

- Np:

  the number of patches

- tol:

  tolerance
