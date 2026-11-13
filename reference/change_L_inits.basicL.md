# Change initial values for `basicL` (**L**)

For the `basicL` module (**L**) for the \\s^{th}\\ species, change
initial values named in `options` leaving other values unchanged.

Initial values of the variable \\L\\ are reset if they are passed as a
named component of `options`

## Usage

``` r
# S3 method for class 'basicL'
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
