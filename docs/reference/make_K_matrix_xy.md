# Make a mosquito dispersal matrix using patch locations

Build a mosquito dispersal matrix using the \\(x\\) and \\(y\\)
locations and a function \\(F_K(d,V)\\) that weights by distance and
perhaps some other variables.

## Usage

``` r
make_K_matrix_xy(xy, F_K = F_exp, V = list())
```

## Arguments

- xy:

  is a vector of the xy-coordinates of patch locations

- F_K:

  is a function that weights putative locations by distance

- V:

  variables

## See also

[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md),
[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md)
