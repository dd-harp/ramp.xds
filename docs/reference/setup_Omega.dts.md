# Make the mosquito demography matrix

Make the mosquito demography matrix

## Usage

``` r
# S3 method for class 'dts'
setup_Omega(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

the derivatives a [vector](https://rdrr.io/r/base/vector.html)

## Note

This method dispatches on the type of `xds_obj$MY_obj[[s]]`
