# Compute egg production (**MY**)

Return the number of eggs laid by the adult mosquito population, per
patch

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

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`.

## Egg Laying Rates

The number of eggs laid by the adult mosquito population, per patch, per
day is the product of:

- \\M\\ — the number of egg-laying mosquitoes, per patch

- \\\nu\\ — the oviposition rate: the number of egg batches laid in each
  patch, per mosquito, per day

- \\\Gamma\\ — the number of eggs per batch \$\$G = \nu M \Gamma\$\$
  Behavioral-state modules use gravid mosquito density in place of `M`.

## See also

[compute_eggs_laid](https://dd-harp.github.io/ramp.xds/reference/compute_eggs_laid.md),
[egg_laying](https://dd-harp.github.io/ramp.xds/reference/egg_laying.md),
[EggLaying](https://dd-harp.github.io/ramp.xds/reference/EggLaying.md)
