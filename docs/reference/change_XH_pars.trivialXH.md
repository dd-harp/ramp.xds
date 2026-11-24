# Change parameters for `trivialXH` (**XH**)

Change parameter values named in `options`, leaving other parameter
values unchanged. Rebuild only forcing functions whose parameters are
named in `options`; an empty parameter list resets the corresponding
function to constant one.

## Usage

``` r
# S3 method for class 'trivialXH'
change_XH_pars(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the host species index

## Value

an **`xds`** model object
