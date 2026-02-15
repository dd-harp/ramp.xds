# Make a Sharkfin Function

A sharkfin function is built in steps:

1.  take the product of two sigmoidal functions

    - the first one rises around day \\D\\ with rate parameter \\uk\\

    - the second one decays around day \\D+L\\ with rate \\-dk\\

2.  the product is raised a power \\pw\\

3.  the result is scaled so that the maximum is \\mx\\ For the default
    values, the function looks like a shark fin.

## Usage

``` r
# S3 method for class 'sharkfin'
make_function(opts)
```

## Arguments

- opts:

  a named list

## Value

a function
