# Handle derivatives for the `trivial` **MYZ** module

Implements
[dMYZdt](https://dd-harp.github.io/ramp.xds/reference/dMYZdt.md) for the
trivial (forced emergence) model.

## Usage

``` r
# S3 method for trivial
dMYZdt(t, y, pars, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an `xds` object

- s:

  the species index

## Value

a null value
