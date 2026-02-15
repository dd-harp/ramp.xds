# Compute the net infectiousness

Implements [F_ni](https://dd-harp.github.io/ramp.xds/reference/F_ni.md)
for the SIS model.

## Usage

``` r
# S3 method for class 'SIS'
F_ni(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
