# Set Up Exposure

Set up the model for exposure. The daily EIR is an expected value, but
that expectation can have a distribution in a population. For example,
if the expectation is gamma distributed, then we would get a negative
binomial distribution of bites per person.

Note that
[Exposure](https://dd-harp.github.io/ramp.xds/reference/Exposure.md)
handles local exposure and exposure while traveling separately.

## Usage

``` r
setup_exposure(EHname, xds_obj, i = 1, options = list())
```

## Arguments

- EHname:

  environmental heterogeneity model name

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

- options:

  set up options list

## Value

an **`xds`** object
