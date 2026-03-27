# Importation (none)

No-op method for the `Importation` junction when no dynamic importation
has been configured. The static default parameters set by
[setup_importation_object](https://dd-harp.github.io/ramp.xds/reference/setup_importation_object.md)
are used as-is.

## Usage

``` r
# S3 method for class 'none'
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
