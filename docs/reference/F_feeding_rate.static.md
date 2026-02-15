# Constant baseline blood feeding rate

Implements
[F_feeding_rate](https://dd-harp.github.io/ramp.xds/reference/F_feeding_rate.md)
for a static model

## Usage

``` r
# S3 method for class 'static'
F_feeding_rate(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\f\\, the baseline blood feeding rate

## Note

This method dispatches on the type of `f_obj` attached to the `MY_obj`.
