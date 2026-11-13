# Set up `SIS` (**XH**)

This sets up an `SIS` model object for the **XH** module for the
\\i^{th}\\ host species

- the object is created by
  [make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md)

- the initial conditions are set by
  [setup_XH_inits](https://dd-harp.github.io/ramp.xds/reference/setup_XH_inits.md)

- `options` is passed to both to override the defaults

## Usage

``` r
# S3 method for class 'SIS'
setup_XH_obj(XHname, residence, xds_obj, options = list(), i = 1)
```

## Arguments

- XHname:

  the model name

- residence:

  the residence vector

- xds_obj:

  an **`xds`** model object

- options:

  model options as a named list

- i:

  the host species index

## Value

an **`xds`** object

## Note

The module object is stored at `xds_obj$XH_obj[[i]]`

## State Variables

- `H`:

  the total human or host population density

- `I`:

  the density of infected humans or hosts

- `S=H-I`:

  the density of susceptible humans or hosts

## Parameters

The parameters that are returned by
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and also modifiable by
[change_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/change_XH_pars.SIS.md)
describe the dynamics, or detection by diagnostic methods.

- `b`:

  the fraction of infective bites that would cause an infection

- `r`:

  clearance rate for infections

- `c`:

  the probability an infected person would infect a mosquito

- `rho` or \\\rho\\:

  the fraction of incident infections that are treated and cured

- `sigma` or \\\sigma\\:

  the rate that prevalent infections are treated and cured

- `q_lm`:

  expected prevalence by light microscopy would be \\q\_{lm} \\x\\

- `q_rdt`:

  expected prevalence by RDT would be \\q\_{rdt} \\x\\

- `q_pcr`:

  expected prevalence by PCR would be \\q\_{pcr} \\x\\

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md),
[make_XH_obj_SIS](https://dd-harp.github.io/ramp.xds/reference/make_XH_obj_SIS.md)
