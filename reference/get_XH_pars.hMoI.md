# Change parameters for `hMoI` (**XH**)

Parameter values for the \\i^{th}\\ host are stored as
`xds_obj$XH_obj[[i]]`. This returns the stored parameter values as a
list.

## Usage

``` r
# S3 method for class 'hMoI'
get_XH_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html)

## Parameters

- `r1` or \\r_1\\:

  clearance rate for all infections

- `r2` or \\r_2\\:

  clearance rate for apparent infections, \\r_2 \> r_1\\

- `b`:

  probability of infection per infectious bite

- `c1` or \\c_1\\:

  probability of transmitting from m1

- `c2` or \\c_2\\:

  probability of transmitting from m2

- H:

  human or host population density

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md),
[make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md)
