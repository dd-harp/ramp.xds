# Compute blood feeding objects: setup for static models

This sets up host available, \\W\\, total blood host available, \\B\\,
the time spent matrix \\\Theta\\, and the time-at-risk matrix \\\Psi\\
for static models.

## Usage

``` r
# S3 method for class 'setup'
BloodFeeding(t, y, xds_obj)
```

## Arguments

- t:

  the time

- y:

  the state variables

- xds_obj:

  an **`xds`** model object

## Value

an `xds` object

## Details

The mixing matrix, \\\beta\\, depends on blood feeding terms, so the
class of `xds_obj$beta` must also be updated, if they are not dynamic,
so
[trigger_setup](https://dd-harp.github.io/ramp.xds/reference/trigger_setup.md)
is called.
