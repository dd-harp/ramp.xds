# Get mean forcing

Get `Lambda`, the mean emergence rate(s) \\(\Lambda\\), for a model
forced by adult mosquito emergence.

## Usage

``` r
# S3 method for class 'Lambda'
get_mean_forcing(xds_obj, ix = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- ix:

  the species index

## Value

a [vector](https://rdrr.io/r/base/vector.html), the value of `Lambda`
for the indexed vector species
