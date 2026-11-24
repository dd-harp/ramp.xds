# Change parameters for `hMoI` (**XH**)

Change parameter values named in `options` for the `hMoI` module,
leaving other values unchanged.

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

[make_XH_obj_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_hMoI.md),
[get_XH_pars.hMoI](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.hMoI.md)

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
