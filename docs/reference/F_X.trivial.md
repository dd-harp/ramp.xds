# Size of effective infectious human population

Implements [F_X](https://dd-harp.github.io/ramp.xds/reference/F_X.md)
for the trivial model

## Usage

``` r
# S3 method for class 'trivial'
F_X(t, y, xds_obj, i)
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
