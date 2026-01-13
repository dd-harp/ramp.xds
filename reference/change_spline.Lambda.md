# Set the \\y\\ value for interpolating points

Set the yy for the seasonal pattern for the \\s^{th}\\ species and
return the **`ramp.xds`** model object

## Usage

``` r
# S3 method for class 'Lambda'
change_spline(X, xds_obj, s = 1)
```

## Arguments

- X:

  new interpolation points, as a list

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the **`ramp.xds`** model object
