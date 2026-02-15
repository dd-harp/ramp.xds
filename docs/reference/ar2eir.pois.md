# Convert AR to EIR under a Poisson model for Exposure

The inverse of
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md)
is \$\$E = -\frac{\log(1-ar)}{b}\$\$

## Usage

``` r
# S3 method for class 'pois'
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

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## See also

Also, see
[F_ar.pois](https://dd-harp.github.io/ramp.xds/reference/F_ar.pois.md)
