# Change season parameters

Change parameters for the seasonality function when
`forced_by = "Lambda"`

## Usage

``` r
# S3 method for class 'Lambda'
change_season(X, xds_obj, ix = 1)
```

## Arguments

- X:

  a list with new parameters for bottom, phase, and pw

- xds_obj:

  an **`xds`** model object

- ix:

  the vector species index

## Value

an **`xds`** object
