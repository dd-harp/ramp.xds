# Difference equations isolating the humans, forced with Ztrace

Compute and update the state variables for a model with only humans

## Usage

``` r
dts_update_MYt(t, y, xds_obj, s)
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

a [vector](https://rdrr.io/r/base/vector.html) containing the vector of
all state derivatives
