# Generalized spatial differential equation model (mosquito only)

Mirrors
[Exogenous](https://dd-harp.github.io/ramp.xds/reference/Exogenous.md)
but only includes the adult and aquatic mosquito components.

## Usage

``` r
# S3 method for mosy
Exogenous(t, y, pars)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

## Value

a [list](https://rdrr.io/r/base/list.html) containing the vector of all
state derivatives
