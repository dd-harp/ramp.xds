# Change initial values (**MY**)

Change initial values named in `options` for the selected **MY** module
and vector species, leaving other values unchanged.

## Usage

``` r
change_MY_inits(xds_obj, options = list(), s = 1)
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

## Note

Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
