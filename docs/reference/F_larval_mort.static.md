# Static model for the blood feeding rate

Implements
[F_larval_mort](https://dd-harp.github.io/ramp.xds/reference/F_larval_mort.md)
for a static model

## Usage

``` r
# S3 method for class 'static'
F_larval_mort(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\phi\\, the baseline human fraction
