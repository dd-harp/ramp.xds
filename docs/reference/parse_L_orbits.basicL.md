# Parse outputs for `basicL` (**L**)

The function extracts the \\(L\\) outputs from the solution matrix and
returns them as a named list.

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
