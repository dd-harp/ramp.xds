# Check egg laying rate

Check that the egg laying rates are in the expected range and length:

- all elements are non-negative

- length(F_nu) == nPatches

## Usage

``` r
check_nu(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the egg laying rate

## See also

[xds_info_egg_laying](https://dd-harp.github.io/ramp.xds/reference/xds_info_egg_laying.md)
