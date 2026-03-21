# Change season parameters

Change parameters for the seasonality function when `forced_by = "none"`

## Usage

``` r
# S3 method for class 'none'
change_season(X, xds_obj, s = 1)
```

## Arguments

- X:

  a list with new parameters for bottom, phase, and pw

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
