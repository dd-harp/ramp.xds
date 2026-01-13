# Set no exogenous health variables

After none setup, no exogenous variables are configured so `Health`
returns the **`ramp.xds`** model object without modification

## Usage

``` r
# S3 method for class 'none'
Health(t, y, xds_obj)
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
