# Check blood feeding rate

Check that blood feeding rates are numeric, between zero and one, and
have length `nPatches`.

## Usage

``` r
check_f(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the validated blood feeding rate

## See also

[xds_info_blood_feeding](https://dd-harp.github.io/ramp.xds/reference/xds_info_blood_feeding.md)
