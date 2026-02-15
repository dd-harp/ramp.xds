# Compute the Local FoI

Compute the daily local FoI as a function of the daily EIR and effects
of partial immunity.

## Usage

``` r
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

the local FoI as a [numeric](https://rdrr.io/r/base/numeric.html) vector
of length \\n_h =\\ `nStrata`

## Details

This function computes the daily force of infection (FoI) as a function
of the daily EIR under a probabilistic model for environmentally
heterogeneous exposure – the distribution of bites per person.

The model for human / host infection (\\\cal X\\) provide a term \\b\\,
from
[F_infectivity](https://dd-harp.github.io/ramp.xds/reference/F_infectivity.md)
that describe the probability of infection per infectious bite, possibly
affected by pre-erythrocytic immunity.

## See also

Cases:
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
& [F_foi.nb](https://dd-harp.github.io/ramp.xds/reference/F_foi.nb.md).
Related:
[Exposure.xde](https://dd-harp.github.io/ramp.xds/reference/Exposure.xde.md)
& [foi2eir](https://dd-harp.github.io/ramp.xds/reference/foi2eir.md)
