# Get parameters for `basicL` (**L**)

Return the parameters for the `basicL` module as a named list.

## Usage

``` r
# S3 method for class 'basicL'
get_L_pars(xds_obj, s = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of parameter values

## Parameters

- `psi` or \\\psi\\:

  maturation rate

- `xi` or \\\xi\\:

  delayed maturation parameter in response to mean crowding

- `phi` or \\\phi\\:

  density-independent mortality rate

- `theta` or \\\theta\\:

  slope of density-dependent mortality

## See also

[make_L_obj_basicL](https://dd-harp.github.io/ramp.xds/reference/make_L_obj_basicL.md),
[change_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.basicL.md)

[dLdt.basicL](https://dd-harp.github.io/ramp.xds/reference/dLdt.basicL.md)
or
[change_L_pars.basicL](https://dd-harp.github.io/ramp.xds/reference/change_L_pars.basicL.md)
