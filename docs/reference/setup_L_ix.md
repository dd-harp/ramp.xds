# Set up variable indices (**L**)

Set up variable indices for the selected **L** module and vector
species.

## Usage

``` r
setup_L_ix(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

an **`xds`** model object

## Note

Dispatch uses the class of `xds_obj$L_obj[[s]]`.

## See also

[get_L_ix](https://dd-harp.github.io/ramp.xds/reference/get_L_ix.md)
