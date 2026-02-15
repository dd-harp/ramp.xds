# Vector Control, Stage 1

A junction to implement stage one vector control functions.

## Usage

``` r
VectorControl1(t, y, xds_obj)
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

Non-trivial vector control modules are in
[**`ramp.control`**](https://github.com/dd-harp/ramp.control).
