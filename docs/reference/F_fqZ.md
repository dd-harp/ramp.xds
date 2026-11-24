# Compute infective mosquito biting density (**MY**)

Return the daily infective mosquito biting density on humans, `fqZ`, for
each patch.

## Usage

``` r
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

## Note

Dispatch on the module object at `xds_obj$MY_obj[[s]]`. This is a
patch-level biting density, not a per-host biting rate.

## See also

[F_fqM](https://dd-harp.github.io/ramp.xds/reference/F_fqM.md)
