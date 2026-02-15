# Number of eggs laid by adult mosquitoes

Implements
[F_eggs](https://dd-harp.github.io/ramp.xds/reference/F_eggs.md) for the
trivial model.

## Usage

``` r
# S3 method for class 'trivial'
F_eggs(t, y, xds_obj, s)
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`
