# Egg production rate for `GeM`

Implements
[F_eggs](https://dd-harp.github.io/ramp.xds/reference/F_eggs.md) for the
GeM model.

## Usage

``` r
# S3 method for class 'GeM'
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

The egg batch rate \\\nu\\ and the number of eggs per batch,
`eggs_per_batch`, determine the egg production rate per patch: \$\$G=\nu
M\cdot\texttt{eggs\\per\\batch}.\$\$ F_eggs.GeM computes \\G\\, which is
passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
for distribution among aquatic habitats.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
