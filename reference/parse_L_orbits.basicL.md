# Parse outputs for `basicL` (**L**)

Extract the model state outputs for the `basicL` module from `outputs`
and return them by name in a list. Return the aquatic mosquito density
`L`.

## Usage

``` r
# S3 method for class 'basicL'
parse_L_orbits(outputs, xds_obj, s)
```

## Arguments

- outputs:

  a [matrix](https://rdrr.io/r/base/matrix.html) of model state outputs

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Note

Variable indices are stored at `xds_obj$L_obj[[s]]$ix`.
