# Compute Local Attack Rates

Compute the local attack rates as a function of the daily EIR and
immunity.

## Usage

``` r
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector: local attack
rates for the strata

## Details

This function computes the daily local attack rates (ARs) as a function
of the daily EIR under a probabilistic model for environmentally
heterogeneous exposure – the distribution of bites per person.

The model for human / host infection (\\\cal X\\) provide a term \\b\\,
from
[F_infectivity](https://dd-harp.github.io/ramp.xds/reference/F_infectivity.md)
that describe the probability of infection per infectious bite, possibly
affected by pre-erythrocytic immunity

## See also

Cases:
[F_ar.nb](https://dd-harp.github.io/ramp.xds/reference/F_ar.nb.md) and
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md).
Related:
[ar2eir](https://dd-harp.github.io/ramp.xds/reference/ar2eir.md) &
[Exposure.dts](https://dd-harp.github.io/ramp.xds/reference/Exposure.dts.md)
