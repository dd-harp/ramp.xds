# Compute Vector Control Effect Sizes

With no vector control, the **`xds`** object is returned unmodified

## Usage

``` r
# S3 method for class 'none'
VectorControl(t, y, xds_obj)
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

## Note

No control or `none` is the default setting
