# Update the mosquito demographic matrix (**MY**)

Retain or recompute the demographic matrix.

## Usage

``` r
update_Omega(xds_obj, s)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  vector species index

## Value

an **`xds`** model object

## Note

Dispatch on the class of the port object `Omega_obj` to retain or
recompute the demographic matrix.
