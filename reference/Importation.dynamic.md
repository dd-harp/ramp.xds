# Importation

Call
[importation_dynamics](https://dd-harp.github.io/ramp.xds/reference/importation_dynamics.md)
to update visitor availability, visitor infectiousness, time away, and
travel EIR for each host species.

## Usage

``` r
# S3 method for class 'dynamic'
Importation(t, y, xds_obj)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** model object
