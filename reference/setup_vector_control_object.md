# Setup the Vector Control Junction

This creates `xds_obj$vector_control` and sets
`class(vector_control) = 'none'.`

Vector control is implemented in two stages.

Functions to implement vector control are in a companion package called
[**`ramp.control`**](https://github.com/dd-harp/ramp.control).

## Usage

``` r
setup_vector_control_object(xds_obj)
```

## Arguments

- xds_obj:

  an **`xds`** model object

## Value

a **`ramp.xds`** model object

## See also

[VectorControl1.none](https://dd-harp.github.io/ramp.xds/reference/VectorControl1.none.md)
