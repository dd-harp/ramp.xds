# Check patch emigration rate

Check that the patch emigration rates are numeric, between zero and one,
and have length `nPatches`.

## Usage

``` r
check_sigma(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the validated patch emigration rates

## See also

[xds_info_mosquito_demography](https://dd-harp.github.io/ramp.xds/reference/xds_info_mosquito_demography.md)
