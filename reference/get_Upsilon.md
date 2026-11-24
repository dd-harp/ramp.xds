# Get the EIP survival-and-dispersal matrix (**MY**)

Return the stored matrix describing mosquito survival and dispersal
through the EIP, \\\Upsilon\\, for the selected species.

## Usage

``` r
get_Upsilon(xds_obj, s = 1)
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

The matrix is stored at `xds_obj$MY_obj[[s]]$Upsilon`. This function
does not recompute the matrix or modify the model object.

## See also

[get_Omega](https://dd-harp.github.io/ramp.xds/reference/get_Omega.md),
[change_Upsilon](https://dd-harp.github.io/ramp.xds/reference/change_Upsilon.md),
[xds_info_mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
