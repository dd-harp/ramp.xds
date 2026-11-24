# Compute the human transmitting capacity

Compute the human transmitting capacity

## Usage

``` r
get_HTC(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

This method dispatches on the type of `xds_obj$XH_obj[[i]]`.
