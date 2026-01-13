# Poisson Attack Rates

A Poisson model for attack rates as a function of the daily EIR.

## Usage

``` r
# S3 method for class 'pois'
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector: attack rates
for the population strata

## Details

This function computes the daily attack rates (\\\alpha\\), the fraction
of a cohort that gets infected in a day, as a function of the daily EIR
(\\E\\) under a Poisson model for the distribution of bites per person,
and the probability of infection per infective bite (\\b\\): \$\$\alpha
= 1 - e^{- b E}\$\$ The expression is equivalent to

`ppois(0, b*eir, lower.tail=FALSE)`

## See also

Related: [F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md)
&
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
&
[ar2eir.pois](https://dd-harp.github.io/ramp.xds/reference/ar2eir.pois.md).
Called from
[Exposure.dts](https://dd-harp.github.io/ramp.xds/reference/Exposure.dts.md)
