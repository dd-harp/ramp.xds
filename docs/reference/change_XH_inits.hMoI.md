# Change initial values for `hMoI` (**XH**)

For the `hMoI` module (**XH**) for the \\(i^{th}\\) species, change
initial values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'hMoI'
change_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the host species index

## Value

an **`xds`** object
