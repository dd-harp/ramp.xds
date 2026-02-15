# Convert AR to EIR

This function computes the inverse of
[F_ar](https://dd-harp.github.io/ramp.xds/reference/F_ar.md) under a
model for environmentally heterogeneous exposure.

## Usage

``` r
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

the daily EIR as a [numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

Cases:
[ar2eir.pois](https://dd-harp.github.io/ramp.xds/reference/ar2eir.pois.md)
& [ar2eir.nb](https://dd-harp.github.io/ramp.xds/reference/ar2eir.nb.md)
