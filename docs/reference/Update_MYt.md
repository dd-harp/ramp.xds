# Update states for an adult mosquito model

This method dispatches on the type of `xds_obj$MY_obj[[s]]`.

## Usage

``` r
Update_MYt(t, y, xds_obj, s)
```

## Arguments

- t:

  current simulation time

- y:

  state vector

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

Updated states for an adult mosquito model, a
[vector](https://rdrr.io/r/base/vector.html)
