# Compute blood feeding objects dynamically

Compute host available, \\W\\, total blood host available, \\B\\, the
time spent matrix \\(\Theta)\\, and the time-at-risk matrix \\(\Psi)\\
for static models.

## Usage

``` r
# S3 method for class 'dynamic'
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
