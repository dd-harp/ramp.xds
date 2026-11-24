# Get the mosquito demographic matrix (**MY**)

Return the stored mosquito demographic matrix, \\\Omega\\, for the
selected species.

## Usage

``` r
get_Omega(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [matrix](https://rdrr.io/r/base/matrix.html) of dimensions `nPatches`
by `nPatches`, or `NULL` if no matrix is stored

## Note

The matrix is stored at `xds_obj$MY_obj[[s]]$Omega`. This function does
not recompute the matrix or modify the model object.

## See also

[get_Upsilon](https://dd-harp.github.io/ramp.xds/reference/get_Upsilon.md),
[change_Omega](https://dd-harp.github.io/ramp.xds/reference/change_Omega.md),
[xds_info_mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
