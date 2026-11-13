# Get parameters for `SIS` (**XH**)

Returns the stored parameter values as a list.

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

SIS model parameters

## Note

Parameter values for the \\i^{th}\\ host are stored as
`xds_obj$XH_obj[[i]]`.

## Parameters

The parameters that are returned by get_XH_pars.SIS and also modifiable
by
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
