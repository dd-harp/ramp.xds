# Static model patch emigration

Implements [F_nu](https://dd-harp.github.io/ramp.xds/reference/F_nu.md)
for a static model

## Usage

``` r
# S3 method for class 'static'
F_nu(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\nu\\, the patch emigration rate
