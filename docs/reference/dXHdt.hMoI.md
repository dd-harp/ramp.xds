# Compute Derivatives for `hMoI` (**X** Module)

Implements
[dXHdt](https://dd-harp.github.io/ramp.xds/reference/dXHdt.md) for the
hybrid MoI model.

## Usage

``` r
# S3 method for class 'hMoI'
dXHdt(t, y, xds_obj, i)
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector
