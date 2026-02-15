# Infection blocking pre-erythrocytic immunity

Implements [F_b](https://dd-harp.github.io/ramp.xds/reference/F_b.md)
for the SIS model.

## Usage

``` r
# S3 method for SIS
F_b(y, pars, i)
```

## Arguments

- y:

  state vector

- pars:

  an **`xds`** object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
