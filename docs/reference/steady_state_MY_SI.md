# Steady states: MY-SI

Select the `SI` steady-state calculation using `xds`. Only the
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

a [list](https://rdrr.io/r/base/list.html)
