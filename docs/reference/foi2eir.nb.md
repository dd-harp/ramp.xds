# A negative binomial model for the daily FoI as a function of the daily EIR.

Implements
[foi2eir](https://dd-harp.github.io/ramp.xds/reference/foi2eir.md) for a
negative binomial model

## Usage

``` r
# S3 method for class 'nb'
foi2eir(foi, b, env_het_obj)
```

## Arguments

- foi:

  the daily FoI for each stratum

- b:

  the probability of infection, per bite

- env_het_obj:

  an environmental heterogeneity model object

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## Details

Compute the daily AR, \\h\\, given the daily EIR, \\E\\, under a
negative binomial model for the distribution of bites per person, and
\\b\\, the probability of infection per infective bite: \$\$\alpha =
\left(e^{h/\phi} -1\right) \frac \phi b \$\$

## See also

[F_foi.nb](https://dd-harp.github.io/ramp.xds/reference/F_foi.nb.md)
