# Set up variable indices (**XH**)

Set up variable indices for the selected **XH** module and host species.

## Usage

``` r
setup_XH_ix(xds_obj, i)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$XH_obj[[i]]`.
