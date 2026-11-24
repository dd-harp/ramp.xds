# Make initial values for `SIS` (**XH**)

Create a named list of initial values for the `SIS` module. Values in
`options` override the defaults.

## Usage

``` r
make_XH_inits_SIS(nStrata, options = list(), H = 1000, I = 1)
```

## Arguments

- nStrata:

  the number of strata in the model

- options:

  a [list](https://rdrr.io/r/base/list.html) to override defaults

- H:

  the initial human population density

- I:

  the initial number of humans infected

## Value

a named [list](https://rdrr.io/r/base/list.html) of initial values

## Details

Initial values must be numeric scalars or have length `nStrata`. Scalars
are expanded to that length.

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md),
[setup_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.SIS.md)

[change_XH_inits.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_inits.SIS.md)
