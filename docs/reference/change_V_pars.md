# Set new V parameter values

Set new V parameter values

## Usage

``` r
change_V_pars(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the auxiliary-variable module index

## Value

an **`xds`** model object

## Note

This method dispatches on the type of `xds_obj$V_obj[[i]]`.
