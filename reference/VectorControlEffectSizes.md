# Apply vector control effect sizes

This calls
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md)
and
[LEffectSizes](https://dd-harp.github.io/ramp.xds/reference/LEffectSizes.md)
for each species. This function applies the effect sizes of vector
control to the bionomic parameters for each vector species.

## Usage

``` r
VectorControlEffectSizes(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

a [list](https://rdrr.io/r/base/list.html)
