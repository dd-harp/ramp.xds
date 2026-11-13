# Change initial values for `trivial` (**L**)

For the `trivial` module (**L**) for the \\(s^{th}\\) species, change
initial values named in `options` leaving other values unchanged.

Returns the unmodified **`xds`** object.

## Usage

``` r
# S3 method for class 'trivial'
change_L_inits(xds_obj, options = list(), s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- s:

  the vector species index

## Value

an **`xds`** object
