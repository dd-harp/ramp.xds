# Compute steady states for `basicM` (**MY**)

Compute steady-state mosquito densities for the `basicM` module given
adult mosquito emergence `Lambda`. Both differential equations and
discrete-time systems are supported.

## Usage

``` r
steady_state_M_basicM(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [numeric](https://rdrr.io/r/base/numeric.html) vector containing
`M` followed by `P`, each with one value per patch

## Note

Dispatch on `xds_obj$xds` to select the `xde` or `dts` method.
