# Set up variable indices for `basicL` (**L**)

Assign consecutive variable indices for the `basicL` module, starting
after `xds_obj$max_ix`, and update the maximum index. Allocate blocks in
the order `L_ix`. Each block has length `nHabitats`.

## Usage

``` r
# S3 method for class 'basicL'
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

Indices are stored at `xds_obj$L_obj[[s]]$ix`. The maximum index is
stored at `xds_obj$max_ix`.
