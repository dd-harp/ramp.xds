# Importation

No-op method for the `Importation` junction when no dynamic importation
has been configured. The static default parameters set by
[setup_importation](https://dd-harp.github.io/ramp.xds/reference/setup_importation.md)
are used as-is.

## Usage

``` r
# S3 method for class 'dynamic'
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
