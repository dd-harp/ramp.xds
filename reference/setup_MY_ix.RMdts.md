# Set up variable indices for `RMdts` (**MY**)

Assign consecutive variable indices for the `RMdts` module, starting
after `xds_obj$max_ix`, and update the maximum index. Allocate blocks in
the order `M_ix`, `P_ix`, `U_ix`, `Y_ix`, `Z_ix`. Each block has length
`nPatches`, except `Y_ix`, which has length
`nPatches * xds_obj$MY_obj[[s]]$max_eip`.

## Usage

``` r
# S3 method for class 'RMdts'
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

Indices are stored at `xds_obj$MY_obj[[s]]$ix`. The maximum index is
stored at `xds_obj$max_ix`.
