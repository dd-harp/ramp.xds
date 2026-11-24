# Change Mosquito Dispersal Matrix

Run
[check_K_matrix](https://dd-harp.github.io/ramp.xds/reference/check_K_matrix.md).

After passing checks, `xds_obj` is updated.

For models with two or more dispersal matrices, use `which_K`.

In models with multiple species, use `s` to specify the species to
update.

## Usage

``` r
# S3 method for class 'K'
change_K_matrix(K_matrix, xds_obj, which_K = "K", s = 1)
```

## Arguments

- K_matrix:

  a mosquito dispersal [matrix](https://rdrr.io/r/base/matrix.html)

- xds_obj:

  an **`xds`** model object

- which_K:

  which K_matrix

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

The argument `which_K` supports behavioral state models for mosquito
ecology with two or more mosquito dispersal matrices. The default "K"
returns `K_matrix`

## See also

[xds_info_mosquito_dispersal](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_dispersal.md);
[setup_K_matrix](https://dd-harp.github.io/ramp.xds/reference/setup_K_matrix.md)
