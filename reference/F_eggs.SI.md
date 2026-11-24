# Compute egg production for `SI` (**MY**)

Return the number of eggs laid by the adult mosquito population, per
patch

## Usage

``` r
# S3 method for class 'SI'
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

- `G`:

  the number of eggs laid, per patch, per day \$\$G = \nu M \Gamma \$\$

\\G\\ is the dynamical term passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
to distribute eggs among the habitats that are found in each patch.

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
