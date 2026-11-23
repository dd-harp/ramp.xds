# Compute the prevalence of infection by light microscopy

Implements
[F_pfpr_by_lm](https://dd-harp.github.io/ramp.xds/reference/F_pfpr_by_lm.md)
for the `hMoI` model.

## Usage

``` r
# S3 method for class 'hMoI'
F_pfpr_by_lm(vars, XH_obj)
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
