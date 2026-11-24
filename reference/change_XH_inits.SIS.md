# Change initial values for `SIS` (**XH**)

Change initial values named in `options` for the `SIS` module, leaving
other values unchanged.

## Usage

``` r
# S3 method for class 'SIS'
change_XH_inits(xds_obj, options = list(), i = 1)
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

## Details

The variable \\S\\ is redundant. It is computed as \\S=H-I.\\ Its
initial value is not stored, so it can't be set or changed.

## Note

Stored initial values for this module are at
`xds_obj$XH_obj[[i]]$inits`.

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md). The variable
names are returned by calling
[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md).
The function
[make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md)
is called during setup.

[setup_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.SIS.md)
