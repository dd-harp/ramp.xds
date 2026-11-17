# Mosquito biting rate for `GeM`

Returns `fqM`, the daily mosquito biting density on humans for each
patch in the GeM model.

## Usage

``` r
# S3 method for class 'GeM'
F_fqM(t, y, xds_obj, s)
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

## Infective Biting

The human biting density, `fqM`, is the number of human bites per patch
per day; the infective biting density, `fqZ`, is the number of
infectious human bites per patch per day. F_fqM.GeM and
[F_fqZ.GeM](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.GeM.md)
compute these quantities as \\f q M\\ and \\f q Z\\, respectively.
[compute_EIR](https://dd-harp.github.io/ramp.xds/reference/compute_EIR.md)
combines infective biting density with the biting distribution matrix
and resident-bite fraction to compute stratum-level `EIR`.
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md) uses
`fqM`, the same matrix, and resident-bite fraction to compute
stratum-level `HBR`.

## See also

[GeM](https://dd-harp.github.io/ramp.xds/reference/GeM.md),
[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md)
