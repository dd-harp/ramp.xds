# Update state variables for `trivial` (**MY**)

Implements
[Update_MYt](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.md)
for the trivial (forced emergence) model.

## Usage

``` r
# S3 method for class 'trivial'
Update_MYt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length 0
