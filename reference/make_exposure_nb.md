# Make a nbson Exposure Model Object

Set up the nbson model for exposure for continuous time models

## Usage

``` r
make_exposure_nb(nStrata, options, sz = 1)
```

## Arguments

- nStrata:

  the number of population strata

- options:

  options to configure negative binomial exposure

- sz:

  the `size` parameter for a negative binomial distribution

## Value

a local exposure model object

## See also

Also, see
[F_foi.nb](https://dd-harp.github.io/ramp.xds/reference/F_foi.nb.md)
