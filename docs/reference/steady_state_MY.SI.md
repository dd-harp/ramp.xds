# Compute steady states for `SI` (**MY**)

Compute steady-state mosquito densities for the `SI` module given adult
mosquito emergence `Lambda` and net infectiousness `kappa`.

## Usage

``` r
# S3 method for class 'SI'
steady_state_MY(Lambda, kappa, xds_obj, s = 1)
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

Delegate to
[steady_state_MY_SI](https://dd-harp.github.io/ramp.xds/reference/steady_state_MY_SI.md),
which dispatches on `xds_obj$xds`.

## See also

[steady_state_MY_SI](https://dd-harp.github.io/ramp.xds/reference/steady_state_MY_SI.md)
