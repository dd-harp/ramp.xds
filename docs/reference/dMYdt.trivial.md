# Handle derivatives for the `trivial` **MY** module

Implements
[dMYdt](https://dd-harp.github.io/ramp.xds/reference/dMYdt.md) for the
trivial (forced emergence) model.

## Usage

``` r
# S3 method for class 'trivial'
dMYdt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a null value
