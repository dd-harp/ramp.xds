# Set up initial values for `SIS` (**XH**)

Set up initial values for the `SIS` module using
[make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md).
Values in `options` override the defaults.

## Usage

``` r
# S3 method for class 'SIS'
setup_XH_inits(xds_obj, options = list(), i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- options:

  a [list](https://rdrr.io/r/base/list.html)

- i:

  the host species index

## Value

an **`xds`** model object

## Note

Initial values are stored at `xds_obj$XH_obj[[i]]$inits`.

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)

[change_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.SIS.md)

[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md)
