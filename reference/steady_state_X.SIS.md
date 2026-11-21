# Compute the steady states for the `SIS` model as a function of the daily FoI

Compute the steady state of the `SIS` model as a function of the daily
FoI, with fixed population density, no mortality, and no mass-treatment
events.

## Usage

``` r
# S3 method for class 'SIS'
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

the steady states as a named [list](https://rdrr.io/r/base/list.html)

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
