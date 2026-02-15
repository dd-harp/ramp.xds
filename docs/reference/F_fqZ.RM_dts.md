# The net blood feeding rate of the infective mosquito population in a patch

Implements
[F_fqZ](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.md) for the
RM_dts model.

## Usage

``` r
# S3 method for RM_dts
F_fqZ(t, y, pars, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  a [list](https://rdrr.io/r/base/list.html)

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
