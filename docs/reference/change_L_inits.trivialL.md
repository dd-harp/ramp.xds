# Change initial values for `trivialL` (**L**)

For the `trivialL` module (**L**) for the \\(s^{th}\\) species, change
initial values named in `options` leaving other values unchanged.

Returns the unmodified **`xds`** object.

## Usage

``` r
# S3 method for class 'trivialL'
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

an **`xds`** model object
