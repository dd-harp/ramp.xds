# Change initial values for `hMoI` (**XH**)

Change initial values named in `options` for the `hMoI` module, leaving
other values unchanged.

It must be true that \\m_1 \> m_2\\, so this condition is checked before
changing the initial conditions, but some pairs of values would not
arise from any history of exposure.

## Usage

``` r
# S3 method for class 'hMoI'
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

## Note

Stored initial values for this module are at
`xds_obj$XH_obj[[i]]$inits`. If `H` is supplied in `options`, the
module's population density is also updated.

## State Variables

- `m1` or \\m_1\\:

  mean MoI for all infections

- `m2` or \\m_2\\:

  mean MoI for apparent (detectable) infections

## See also

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)

[make_XH_inits_hMoI](https://dd-harp.github.io/ramp.xds/reference/make_XH_inits_hMoI.md),
[setup_XH_inits.hMoI](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.hMoI.md)
