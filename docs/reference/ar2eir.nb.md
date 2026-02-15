# A negative binomial model for the daily EIR. as a function of the daily attack rate

Implements
[ar2eir](https://dd-harp.github.io/ramp.xds/reference/ar2eir.md) for a
negative binomial model

## Usage

``` r
# S3 method for class 'nb'
ar2eir(ar, b, env_het_obj)
```

## Arguments

- ar:

  the attack rate

- b:

  the probability of infection, per bite

- env_het_obj:

  an environmental heterogeneity model object

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## See also

[F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md)
