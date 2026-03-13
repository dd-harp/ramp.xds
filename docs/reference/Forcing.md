# Forcing

Forcing is a
[junction](https://dd-harp.github.io/ramp.xds/reference/junction.md)
that called by
[xds_compute_terms](https://dd-harp.github.io/ramp.xds/reference/xds_compute_terms.md).

Set up a junction for exogenous forcing by weather and hydrology.
Non-trivial examples are in the satellite package `ramp.forcing`

## Usage

``` r
Forcing(t, xds_obj)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
