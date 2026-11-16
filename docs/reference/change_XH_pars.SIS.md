# Change parameters for `SIS` (**XH**)

For the `SIS` module (**XH**) for the \\(i^{th}\\) host species, change
parameter values named in `options` leaving other values unchanged.

## Usage

``` r
# S3 method for class 'SIS'
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

an **`xds`** object

## Parameters

The parameters that are returned by
[get_XH_pars.SIS](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and also modifiable by change_XH_pars.SIS describe the dynamics, or
detection by diagnostic methods.

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

- `q_lm`:

  test positivity by light microscopy

- `q_rdt`:

  test positivity by RDT

- `q_pcr`:

  test positivity by PCR

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md),
[`get_XH_pars.SIS()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and the vignette on [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
