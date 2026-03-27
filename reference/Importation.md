# Importation Junction

Dispatches on `class(xds_obj$importation_obj)` to update
importation-related parameters: time at home, travel EIR, visitor
availability, and visitor infectiousness.

## Usage

``` r
Importation(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
