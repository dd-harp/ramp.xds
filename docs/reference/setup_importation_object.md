# Setup the Importation Object

Setup an object to handle malaria importation through travel and
visitors. The parameters describing available visitors and their
infectiousness, and time away are initialized during setup of the
**XY**-interfracte. This initializes the junction and zero-initializes
travel_EIR.

## Usage

``` r
setup_importation_object(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

an **`xds`** object
