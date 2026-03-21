# Apply vector control effect sizes

Implements
[MEffectSizes](https://dd-harp.github.io/ramp.xds/reference/MEffectSizes.md)
for models with no exogenous forcing

## Usage

``` r
# S3 method for class 'GeM'
MEffectSizes(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
