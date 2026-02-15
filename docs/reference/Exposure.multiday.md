# Exposure and Infection

This function translates seven days of daily entomological inoculation
rate (dEIR) into a multiday attack rate. The daily FoI is the sum of two
terms: 1) a function
[F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md) computes
the local dFoI; 2) a function
[travel_eir](https://dd-harp.github.io/ramp.xds/reference/travel_eir.md)
and
[traveling](https://dd-harp.github.io/ramp.xds/reference/traveling.md)
exposure while traveling.

## Usage

``` r
# S3 method for multiday
Exposure(t, y, pars)
```

## Arguments

- t:

  the time

- y:

  the state variables

- pars:

  an **`xds`** object

## Value

the function modifies **pars** and returns it: the computed FoI are
stored as `pars$FoI`
