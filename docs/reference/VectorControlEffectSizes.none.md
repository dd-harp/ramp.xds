# Set the values of exogenous variables

After none setup, no exogenous variables are configured so the function
returns the unmodified **`xds`** object

## Usage

``` r
# S3 method for class 'none'
VectorControlEffectSizes(t, y, xds_obj)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

## Value

a **`ramp.xds`** model object
