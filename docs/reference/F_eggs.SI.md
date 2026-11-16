# Compute component egg production rates for `SI`

The density of adult mosquitoes is \\(M\\). The **parameters**
describing egg laying by adult mosquitoes are:

- \\(\nu\\) or `nu` is the egg batch rate

- \\(\Gamma\\) or `eggsPerBatch` is the number of eggs per batch

The egg production rate per patch is \$\$\nu \Gamma M\$\$

## Usage

``` r
# S3 method for class 'SI'
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

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nPatches`

## Egg Laying

- `G`:

  the number of eggs laid, per patch, per day \$\$G = \nu M \Gamma \$\$

\\(G\\) is the dynamical term passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
to distribute eggs among the habitats that are found in each patch.

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md)
