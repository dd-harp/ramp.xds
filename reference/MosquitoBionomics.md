# Compute mosquito bionomic parameters

This calls
[MBionomics](https://dd-harp.github.io/ramp.xds/reference/MBionomics.md)
and
[LBionomics](https://dd-harp.github.io/ramp.xds/reference/LBionomics.md)
for each species.

This function computes the bionomic parameters for each vector species,
before any vector control effect sizes are applied. In some models,
these parameters are computed as a function of resource availability.

## Usage

``` r
MosquitoBionomics(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object
