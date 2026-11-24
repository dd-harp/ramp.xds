# Change initial values (**XH**)

Change initial values named in `options` for the selected **XH** module
and host species, leaving other values unchanged.

## Usage

``` r
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

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$XH_obj[[i]]`.

## See also

[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
