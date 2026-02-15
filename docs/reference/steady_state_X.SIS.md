# Compute the steady states for the SIS model as a function of the daily EIR

Compute the steady state of the SIS model as a function of the daily
eir.

## Usage

``` r
# S3 method for class 'SIS'
steady_state_X(foi, H, xds_obj, i = 1)
```

## Arguments

- foi:

  the daily FoI

- H:

  human / host population density

- xds_obj:

  an **`xds`** model object

- i:

  the vector species index

## Value

the steady states as a named vector
