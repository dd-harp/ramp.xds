# Return the parameters as a list

Return the parameters as a list

## Usage

``` r
get_V_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the auxiliary-variable module index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

This method dispatches on the type of `xds_obj$V_obj[[i]]`.
