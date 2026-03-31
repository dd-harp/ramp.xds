# Static model for the blood feeding rate

Implements [F_mu](https://dd-harp.github.io/ramp.xds/reference/F_mu.md)
for a static model

## Usage

``` r
# S3 method for class 'static'
F_mu(t, xds_obj, s)
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
