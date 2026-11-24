# Compute egg production for `macdonald` (**MY**)

Return the number of eggs laid by the adult mosquito population, per
patch

## Usage

``` r
# S3 method for class 'macdonald'
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

The oviposition rate \\(\nu\\) and the number of eggs per batch
\\(\Gamma\\) determine the egg production rate per patch:
\$\$G=\nu\Gamma M.\$\$ F_eggs.macdonald computes \\(G\\), which is
passed to
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
for distribution among aquatic habitats.

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
