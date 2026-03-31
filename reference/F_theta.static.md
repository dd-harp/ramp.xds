# Compute theta (density dependent mortality)

Implements
[F_theta](https://dd-harp.github.io/ramp.xds/reference/F_theta.md) for a
static model

## Usage

``` r
# S3 method for class 'static'
F_theta(t, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

\\theta\\, slope of density-dependent mortality
