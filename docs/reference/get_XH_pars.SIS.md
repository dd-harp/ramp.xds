# Get parameters for `SIS` (**XH**)

Return the parameters for the `SIS` module as a named list.

## Usage

``` r
# S3 method for class 'SIS'
get_XH_pars(xds_obj, i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the host species index

## Value

a named [list](https://rdrr.io/r/base/list.html) of parameter values

## Note

Parameter values for the \\(i^{th}\\) host are stored as
`xds_obj$XH_obj[[i]]`.

## Parameters

The parameters that are returned by get_XH_pars.SIS and also modifiable
by
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)
describe the dynamics or test positivity.

- `b`:

  the fraction of infective bites that would cause an infection

- `r`:

  clearance rate for infections

- `c`:

  the probability an infected person would infect a mosquito

- `rho` or \\(\rho\\):

  the fraction of incident infections that are treated and cured

- `sigma` or \\(\sigma\\):

  the rate that prevalent infections are treated and cured

- `q`:

  test positivity

## See also

[make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md),
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
