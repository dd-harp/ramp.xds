# Difference equations isolating the humans, forced with Ztrace

Compute and update the state variables for a model with only humans

## Usage

``` r
dts_update_MYZt(t, y, pars, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the vector species index

## Value

a [vector](https://rdrr.io/r/base/vector.html) containing the vector of
all state derivatives
