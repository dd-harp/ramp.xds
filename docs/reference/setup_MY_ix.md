# Set up variable indices (**MY**)

Set up variable indices for the selected **MY** module and vector
species.

## Usage

``` r
setup_MY_ix(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$MY_obj[[s]]`.
