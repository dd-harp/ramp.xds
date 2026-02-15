# Compute *Pf*PR by light microscopy

Implements
[F_prevalence](https://dd-harp.github.io/ramp.xds/reference/F_prevalence.md)
for the SIS model.

## Usage

``` r
# S3 method for class 'SIS'
F_pfpr_by_lm(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
