# Mosquito bionomics (**L**)

Compute baseline bionomic parameter values for an **L** component
module. Store each port-backed parameter `x` as `x_t` and reset `es_x`
to 1. Parameters without ports are stored directly as `x`.

## Usage

``` r
LBionomics(t, y, xds_obj, s)
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

[LEffectSizes](https://dd-harp.github.io/ramp.xds/reference/LEffectSizes.md),
[MosquitoBionomics](https://dd-harp.github.io/ramp.xds/reference/MosquitoBionomics.md)
