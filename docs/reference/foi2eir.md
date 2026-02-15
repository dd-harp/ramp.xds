# Convert FoI to EIR

This function computes the inverse of
[F_foi](https://dd-harp.github.io/ramp.xds/reference/F_foi.md) under a
model for environmentally heterogeneous exposure.

## Usage

``` r
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

the daily EIR as a [numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

Cases:
[foi2eir.pois](https://dd-harp.github.io/ramp.xds/reference/foi2eir.pois.md)
&
[foi2eir.nb](https://dd-harp.github.io/ramp.xds/reference/foi2eir.nb.md)
