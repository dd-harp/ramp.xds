# Compute steady states for `SI` (**MY**)

Compute steady-state mosquito densities for the `SI` module given adult
mosquito emergence `Lambda` and net infectiousness `kappa`. Only the
differential-equation calculation is currently implemented.

## Usage

``` r
steady_state_MY_SI(Lambda, kappa, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) with elements `M`, `P`,
and `Y`, each containing one value per patch

## Note

Dispatch on `xds_obj$xds`; only the `xde` method is implemented.
