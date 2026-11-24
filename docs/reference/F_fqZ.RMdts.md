# Compute infective mosquito biting density for `RMdts` (**MY**)

Return the daily infective mosquito biting density on humans, `fqZ`, for
each patch in the `RMdts` module. Compute `fqZ = f*q*Z`.

## Usage

``` r
# S3 method for class 'RMdts'
F_fqZ(t, y, xds_obj, s)
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

[F_fqM.RMdts](https://dd-harp.github.io/ramp.xds/reference/F_fqM.RMdts.md)
