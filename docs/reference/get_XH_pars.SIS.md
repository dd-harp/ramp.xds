# Get *SIS* module parameters

Returns the stored parameter values as a list.

## Usage

``` r
# S3 method for class 'SIS'
get_XH_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

SIS model parameters

## Note

Parameter values for the \\i^{th}\\ host are stored as
`xds_obj$XH_obj[[i]]`.
