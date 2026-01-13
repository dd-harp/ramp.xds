# Compute Infectivity

This function computes the fraction of infectious bites that cause an
infection. The SIS model, assumes a constant fraction causes infection,
\\b\\.

## Usage

``` r
# S3 method for class 'SIS'
F_infectivity(y, xds_obj, i)
```

## Arguments

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

The constant \\b\\
