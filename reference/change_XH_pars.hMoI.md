# Get parameters for `hMoI` (**XH**)

For the `hMoI` module (**XH**) for the \\i^{th}\\ species, change
parameter values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'hMoI'
change_XH_pars(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a named list

- i:

  the host species index

## Value

an **`xds`** model object

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

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
