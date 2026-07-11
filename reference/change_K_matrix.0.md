# Change Mosquito Dispersal Matrix

Check that `K_matrix`

- is an `nPatches` \\\times\\ `nPatches` matrix;

- diagonal elements are -1;

- and columns sum to 0.

After passing checks, `xds_obj` is updated.

In models with multiple species, use `s` to specify the species to
update.

## Usage

``` r
# S3 method for class '`0`'
change_K_matrix(K_matrix, xds_obj, behave = "0", s = 1)
```

## Arguments

- K_matrix:

  a mosquito dispersal [matrix](https://rdrr.io/r/base/matrix.html)

- xds_obj:

  an **`xds`** model object

- behave:

  the behavioral state

- s:

  the vector species index

## Value

an **`xds`** object

## See also

[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md);
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
