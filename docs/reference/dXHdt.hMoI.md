# Compute derivatives for `hMoI` (**XH**)

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

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
