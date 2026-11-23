# Egg production rate

Compute the number of eggs laid by adult mosquitoes, per patch, per day.

## Usage

``` r
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

## Egg Laying Rates

The number of eggs laid by the adult mosquito population, per patch, per
day is the product of:

- \\(M\\) — the number of egg-laying mosquitoes, per patch

- \\(\nu\\) — the oviposition rate: the number of egg batches laid in
  each patch, per mosquito, per day

- \\(\Gamma\\) — the number of eggs per batch \$\$G = \nu M \Gamma\$\$

## See also

[compute_eggs_laid](https://dd-harp.github.io/ramp.xds/reference/compute_eggs_laid.md),
[egg_laying](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md)
