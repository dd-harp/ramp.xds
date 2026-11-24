# Set up variable indices for `SIS` (**XH**)

Assign consecutive variable indices for the `SIS` module, starting after
`xds_obj$max_ix`, and update the maximum index. Allocate blocks in the
order `H_ix`, `I_ix`. Each block has length `nStrata[i]`.

## Usage

``` r
# S3 method for class 'SIS'
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

Indices are stored at `xds_obj$XH_obj[[i]]$ix`. The maximum index is
stored at `xds_obj$max_ix`.

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
