# Infection blocking pre-erythrocytic immunity

Implements
[F_infectivity](https://dd-harp.github.io/ramp.xds/reference/F_infectivity.md)
for the trivial model.

## Usage

``` r
# S3 method for class 'trivial'
F_infectivity(y, xds_obj, i)
```

## Arguments

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
