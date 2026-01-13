# Size of effective infectious human population

The trace function for \\\kappa\\ follows the **`ramp.xds`** standard
for time series forcing. The trace function for \\H\\ can be modified to
follow a trend. Here, \\I = \kappa H\\.

## Usage

``` r
# S3 method for class 'trivial'
F_I(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
