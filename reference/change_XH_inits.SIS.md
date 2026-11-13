# Change initial values for `SIS` (**XH**)

For the `SIS` module (**XH**) for the \\i^{th}\\ species, change initial
values named in `options` leaving other values unchanged.

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

an **`xds`** object

## Details

The variable \\S\\ is redundant. It is computed as \\S=H-I.\\ It's
initial value is not stored, so it can't be set or changed.

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## See also

The variable names are returned by calling
[get_XH_inits](https://dd-harp.github.io/ramp.xds/reference/get_XH_inits.md).
The function
[make_XH_inits_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_SIS.md)
is called during setup.
