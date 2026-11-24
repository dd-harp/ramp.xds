# Compute *Pf*PR by PCR

Implements
[F_pfpr_by_pcr](https://dd-harp.github.io/ramp.xds/reference/F_pfpr_by_pcr.md)
for the `SIS` model.

## Usage

``` r
# S3 method for class 'SIS'
F_pfpr_by_pcr(vars, XH_obj)
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

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
