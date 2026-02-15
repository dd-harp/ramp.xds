# Negative Binomial Attack Rates

A negative binomial model for the attack rate as a function of the daily
EIR.

## Usage

``` r
# S3 method for class 'nb'
F_ar(eir, b, env_het_obj)
```

## Arguments

- eir:

  the daily eir for each stratum

- b:

  the probability of infection, per bite

- env_het_obj:

  an environmental heterogeneity model object

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## Details

This function computes the daily AR, \\\alpha\\, as a function of the
daily EIR, \\E\\ or `eir`, under a negative binomial model for the
distribution of bites per person, and \\b\\, the probability of
infection per infective bite: \$\$\alpha = 1-(1 + b E/\phi)^{-\phi} \$\$
This negative binomial model uses the **mu** \\= bE\\ and **size**
\\=\phi\\ parameterization (see
[pnbinom](https://rdrr.io/r/stats/NegBinomial.html)). The formula is
equivalent to:

`pnbinom(0, mu=b*eir, size=phi, lower.tail=FALSE)`

## See also

Related topics:
[Exposure.dts](https://dd-harp.github.io/ramp.xds/reference/Exposure.dts.md),
[make_exposure_nb](https://dd-harp.github.io/ramp.xds/reference/make_exposure_nb.md)
& [F_foi.nb](https://dd-harp.github.io/ramp.xds/reference/F_foi.nb.md)
