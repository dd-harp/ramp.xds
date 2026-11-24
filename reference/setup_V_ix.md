# Add indices for auxiliary variables to the parameter list

Add indices for auxiliary variables to the parameter list

## Usage

``` r
setup_V_ix(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the auxiliary-variable module index

## Value

an **`xds`** model object

## Note

This method dispatches on the type of `xds_obj$V_obj[[i]]`.
