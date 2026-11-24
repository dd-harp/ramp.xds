# Infective mosquito biting rate

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
