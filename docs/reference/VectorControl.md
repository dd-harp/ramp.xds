# Vector Control

The generic function to implement vector control.

## Usage

``` r
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

This a junction to implement various modes of vector control.
Non-trivial vector control modules are in
[**`ramp.control`**](https://github.com/dd-harp/ramp.control).
