# Compute the prevalence of infection by RDT

Implements
[F_pfpr_by_rdt](https://dd-harp.github.io/ramp.xds/reference/F_pfpr_by_rdt.md)
for the `hMoI` model.

## Usage

``` r
# S3 method for class 'hMoI'
F_pfpr_by_rdt(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for humans

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
