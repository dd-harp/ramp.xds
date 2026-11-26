# Check the model components

Run consistency checks before solving or analyzing a model.

## Usage

``` r
check_models(xds_obj, t = 0)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- t:

  a vector of times

## Value

an **`xds`** model object

## Note

This dispatches on `xds_obj$frame`
