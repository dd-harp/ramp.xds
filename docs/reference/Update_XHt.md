# Update X states for a discrete time system

This method dispatches on the type of `xds_obj$XH_obj[[i]]`.

## Usage

``` r
Update_XHt(t, y, xds_obj, i)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector
