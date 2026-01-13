# Compute Infection Rates

Compute the force of infection (FoI) or attack rates (AR) as a function
of the local daily entomological inoculation rate (dEIR), immunity, and
exposure to malaria while traveling.

## Usage

``` r
Exposure(t, y, xds_obj)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object

## See also

Cases:
[Exposure.xde](https://dd-harp.github.io/ramp.xds/reference/Exposure.xde.md)
&
[Exposure.dts](https://dd-harp.github.io/ramp.xds/reference/Exposure.dts.md).
Related: [F_ar](https://dd-harp.github.io/ramp.xds/reference/F_ar.md) &
[F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md)
