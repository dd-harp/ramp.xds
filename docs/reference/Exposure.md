# Exposure

The EIR computed by
[Transmission](https://dd-harp.github.io/ramp.xds/reference/Transmission.md)
is a measure of *local* risk: it is the expected number of infected
bites, per person, and it is computed for every population stratum. The
exposure module handles two aspects of exposure: travel and
environmental heterogeneity. Exposure transforms the EIR into the FoI

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

## (Environmental Heterogeneity)[environmental_heterogeneity](https://dd-harp.github.io/ramp.xds/reference/environmental_heterogeneity.md)

While EIR is an expected value, the expected value can have a
distribution in a human population stratum. For example, if the
expectation has a Gamma distribution, then the expected number of bites
per person would be negatively binomially distributed.

## Travel Malaria

Write me.

## Pre-Erythrocytic Immunity

Depending on the X module, a parameter called \\b\\ describes the
probability of infection per infectious bite.
