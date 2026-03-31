# Compute phi, density independent mortality rate

Implements
[F_phi](https://dd-harp.github.io/ramp.xds/reference/F_phi.md) for a
static model

## Usage

``` r
# S3 method for class 'static'
F_phi(t, xds_obj, s)
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
