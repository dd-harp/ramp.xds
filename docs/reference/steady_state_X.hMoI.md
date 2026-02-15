# Compute Steady States for `hMoI` (**X**-Model)

Compute the steady state of the hMoI model as a function of the daily
foi

## Usage

``` r
# S3 method for class 'hMoI'
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
