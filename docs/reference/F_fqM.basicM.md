# Mosquito biting rate for `basicM`

Returns `fqM`, the daily mosquito biting density on humans for each
patch in the basicM model.

## Usage

``` r
# S3 method for class 'basicM'
F_fqM(t, y, xds_obj, s)
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

## Infective Biting

The `basicM` module does not represent mosquito infection, so its
infective biting density \\(fqZ\\) is zero. Its human biting density,
\\(fqM\\), is the number of bites on humans in each patch per day.
F_fqM.basicM computes this patch-level quantity.
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) combines
`fqM` with the biting distribution matrix and resident-bite fraction to
compute the stratum-level human biting rate (`HBR`). The `EIR` is zero
for this module because there are no infective bites.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md),
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md)
