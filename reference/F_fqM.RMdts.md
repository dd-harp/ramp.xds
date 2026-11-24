# Compute mosquito biting density for `RMdts` (**MY**)

Return the daily mosquito biting density on humans, `fqM`, for each
patch in the `RMdts` module. Compute `fqM = f*q*M`.

## Usage

``` r
# S3 method for class 'RMdts'
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

## See also

[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md),
[F_fqZ.RMdts](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.RMdts.md)
