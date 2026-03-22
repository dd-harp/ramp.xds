# Compute mosquito bionomic parameters

This calls MBionomics and LBionomics for each species.

This function computes the bionomic parameters for each vector species,
before any vector control effect sizes are applied. In some models,
these parameters are computed as a function of resource availability.

## Usage

``` r
MosquitoBionomics(t, y, xds_obj)
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
