# Set up variable indices for `macdonald` (**MY**)

Assign consecutive variable indices for the `macdonald` module, starting
after `xds_obj$max_ix`, and update the maximum index. Allocate blocks in
the order `M_ix`, `P_ix`, `Y_ix`, `Z_ix`, `kappa_ix`. Each block has
length `nPatches`.

## Usage

``` r
# S3 method for class 'macdonald'
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

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes

- `Y`:

  density of infected adult mosquitoes

- `Z`:

  density of infectious adult mosquitoes

## See also

[macdonald](https://dd-harp.github.io/ramp.xds/reference/macdonald.md)
