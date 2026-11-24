# Compute mosquito biting density (**MY**)

Return the daily mosquito biting density on humans, `fqM`, for each
patch.

## Usage

``` r
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

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`. This is a
patch-level biting density, not a per-host biting rate.

## See also

[F_hbr](https://dd-harp.github.io/ramp.xds/reference/F_hbr.md),
[F_fqZ](https://dd-harp.github.io/ramp.xds/reference/F_fqZ.md)
