# Size of the human population

Implements [F_H](https://dd-harp.github.io/ramp.xds/reference/F_H.md)
for the hybrid MoI model.

## Usage

``` r
# S3 method for class 'hMoI'
F_H(t, y, xds_obj, i)
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
