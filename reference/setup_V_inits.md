# Set up initial values for **V** modules

Set up initial values for **V** modules

## Usage

``` r
setup_V_inits(xds_obj, H, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- H:

  initial host population density

- options:

  a [list](https://rdrr.io/r/base/list.html)

- i:

  the auxiliary-variable module index

## Value

an **`xds`** model object

## Note

This method dispatches on `xds_obj$V_obj[[i]]`.
