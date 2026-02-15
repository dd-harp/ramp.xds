# Size of effective infectious human population

Implements [F_I](https://dd-harp.github.io/ramp.xds/reference/F_I.md)
for the hybrid MoI model.

## Usage

``` r
# S3 method for class 'hMoI'
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
