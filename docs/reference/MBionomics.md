# Mosquito bionomics (**MY**)

Compute the adult mosquito bionomic parameter values. Store each
port-backed parameter `x` as `x_t` and reset `es_x` to 1. Parameters
without ports are stored directly as `x`.

## Usage

``` r
MBionomics(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## See also

[MosquitoBionomics](https://dd-harp.github.io/ramp.xds/reference/MosquitoBionomics.md)
