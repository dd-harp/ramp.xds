# Change y values for spline interpolation points

Set new \\y\\-values for the spline interpolating points and update
`F_trend` when `forced_by = "Lambda"`

## Usage

``` r
# S3 method for class 'Lambda'
change_spline_y(yy, xds_obj, s = 1)
```

## Arguments

- yy:

  new y values for the interpolation points

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** object
