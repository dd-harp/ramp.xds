# Negative Binomial Exposure

Compute the daily FoI from the daily EIR under a negative binomial model
for exposure

## Usage

``` r
# S3 method for class 'nb'
F_foi(eir, b, env_het_obj)
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

This function computes the local daily FoI, \\h\\ as a function of the
local daily EIR, \\E\\, under a negative binomial model for the
distribution of bites per person, and \\b\\, the probability of
infection per infective bite: \$\$h = \phi \log{\frac{1 + b E}\phi}\$\$
The expression is derived by solving the expression in
[F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md) for
\\E\\ This negative binomial model uses the **mu** \\= bE\\ and **size**
\\=\phi\\ parameterization (see
[pnbinom](https://rdrr.io/r/stats/NegBinomial.html)).

## See also

Related topics:
[Exposure.xde](https://dd-harp.github.io/ramp.xds/reference/Exposure.xde.md)
and [F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md)
and
[make_exposure_nb](https://dd-harp.github.io/ramp.xds/reference/make_exposure_nb.md)
