# Convert FoI to EIR under a Poisson model for Exposure

The inverse of
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
is \\E=h/b\\

## Usage

``` r
# S3 method for class 'pois'
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

dEIR as a [numeric](https://rdrr.io/r/base/numeric.html) vector of
length \\n_h=\\`nStrata`

## See also

Also, see
[F_foi.pois](https://dd-harp.github.io/ramp.xds/reference/F_foi.pois.md)
