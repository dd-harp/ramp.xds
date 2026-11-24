# Compute steady states for `hMoI` (**XH**)

Compute steady-state infection values for the `hMoI` module given daily
FoI `foi` for the differential-equation system. The fixed population
input `H` is not used in this calculation.

## Usage

``` r
# S3 method for class 'hMoI'
steady_state_X(foi, H, xds_obj, i = 1)
```

## Arguments

- foi:

  the daily FoI

- H:

  human / host population density

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [numeric](https://rdrr.io/r/base/numeric.html) vector containing
`m1` followed by `m2`

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
