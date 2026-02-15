# Static model for the blood feeding rate

Implements
[F_dispersal_loss](https://dd-harp.github.io/ramp.xds/reference/F_dispersal_loss.md)
for a static model

## Usage

``` r
# S3 method for class 'static'
F_dispersal_loss(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\mu\\, the baseline human fraction
