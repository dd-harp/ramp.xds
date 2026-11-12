# Get parameters for `SIS` (**XH**)

For the `SIS` module (**XH**) for the \\(i^{th}\\) species, change
parameter values named in `options` leaving other values unchanged.

`options` is a named list, and:

- \\(b\\) is set to `options$b`

- \\(c\\) is set to `options$c`

- \\(r\\) is set to `options$r`

- \\(q\_{lm}\\) is set to `options$q_lm`

- \\(q\_{rdt}\\) is set to `options$q_rdt`

- \\(q\_{pcr}\\) is set to `options$q_pcr`

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

## See also

[`get_XH_pars.SIS()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and the github.io essay on [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
