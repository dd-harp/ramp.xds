# Compute mosquito bionomic parameters

Implements
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
for models with no exogenous forcing

## Usage

``` r
# S3 method for class 'GeM'
MBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** object
