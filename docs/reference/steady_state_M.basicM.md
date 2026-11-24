# Compute steady states for `basicM` (**MY**)

Compute steady-state mosquito densities for the `basicM` module given
adult mosquito emergence `Lambda`.

## Usage

``` r
# S3 method for class 'basicM'
steady_state_M(Lambda, xds_obj, s = 1)
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

Delegate to
[steady_state_M_basicM](https://dd-harp.github.io/ramp.xds/reference/steady_state_M_basicM.md),
which dispatches on `xds_obj$xds`.

## See also

[steady_state_M_basicM](https://dd-harp.github.io/ramp.xds/reference/steady_state_M_basicM.md)
