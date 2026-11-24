# Set up variable indices for `SI` (**MY**)

Assign consecutive variable indices for the `SI` module, starting after
`xds_obj$max_ix`, and update the maximum index. Allocate blocks in the
order `M_ix`, `P_ix`, `Y_ix`. Each block has length `nPatches`.

## Usage

``` r
# S3 method for class 'SI'
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

## See also

[SI](https://dd-harp.github.io/ramp.xds/reference/SI.md)
