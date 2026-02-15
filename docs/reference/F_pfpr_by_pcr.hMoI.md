# Compute the prevalence of infection by PCR

Implements
[F_prevalence](https://dd-harp.github.io/ramp.xds/reference/F_prevalence.md)
for the hMoI model.

## Usage

``` r
# S3 method for class 'hMoI'
F_pfpr_by_pcr(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for human

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`
