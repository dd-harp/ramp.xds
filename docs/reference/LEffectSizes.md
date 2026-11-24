# Apply effect sizes (**L**)

Apply vector control effect sizes to baseline aquatic mosquito bionomic
parameter values. For each port-backed parameter, set `x = x_t * es_x`.
Leave parameters without ports unchanged.

## Usage

``` r
LEffectSizes(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** model object

## Note

Dispatch on the module object at `xds_obj$L_obj[[s]]`.

## See also

[LBionomics](https://dd-harp.github.io/ramp.xds/reference/LBionomics.md),
[VectorControlEffectSizes](https://dd-harp.github.io/ramp.xds/reference/VectorControlEffectSizes.md)
