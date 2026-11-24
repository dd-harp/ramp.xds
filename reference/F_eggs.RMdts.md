# Compute egg production for `RMdts` (**MY**)

Return the number of eggs laid by the adult mosquito population, per
patch

## Usage

``` r
# S3 method for class 'RMdts'
F_eggs(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Egg Laying

The daily number of eggs laid per patch is `M*nu*eggsPerBatch`, where
`M` is total adult mosquito density, `nu` is the oviposition rate, and
`eggsPerBatch` is the number of eggs per batch. F_eggs.RMdts returns
this quantity before distribution among aquatic habitats by
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md).

## See also

[RMdts](https://dd-harp.github.io/ramp.xds/reference/RMdts.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
