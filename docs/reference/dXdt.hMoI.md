# Compute Derivatives for the `hMoI` (**X**-Model)

Implements [dXdt](https://dd-harp.github.io/ramp.xds/reference/dXdt.md)
for the hybrid MoI model.

## Usage

``` r
# S3 method for hMoI
dXdt(t, y, pars, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
