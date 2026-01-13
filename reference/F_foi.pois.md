# Poisson Force of Infection

A Poisson model for the force of infection as a function of the daily
EIR.

## Usage

``` r
# S3 method for class 'pois'
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

This function computes the daily FoI (\\h\\) as function of the local
daily EIR (\\E\\) under a Poisson model for the distribution of bites
per person, and the probability of infection per infective bite (\\b\\):
\$\$h = b E\$\$

## See also

Related: [F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md)
& [F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md)
&
[foi2eir.pois](https://dd-harp.github.io/ramp.xds/reference/foi2eir.pois.md).
Called from
[Exposure.xde](https://dd-harp.github.io/ramp.xds/reference/Exposure.xde.md)
