# Difference equations isolating the humans, forced with Ztrace

Compute and update the state variables for a model with only humans

## Usage

``` r
dts_update_XHt(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [vector](https://rdrr.io/r/base/vector.html) containing the vector of
all state derivatives
