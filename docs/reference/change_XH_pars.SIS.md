# Set new values for the SIS model

A utility to change parameters values for the SIS model (**XH**
component)

`options` is a named list, and:

- \\b\\ is set to `options$b`

- \\c\\ is set to `options$c`

- \\r\\ is set to `options$r`

- \\q_lm\\ is set to `options$q_lm`

- \\q_rdt\\ is set to `options$q_rdt`

- \\q_pcr\\ is set to `options$q_pcr`

## Usage

``` r
# S3 method for class 'SIS'
change_XH_pars(xds_obj, i = 1, options = list())
```

## Arguments

- xds_obj:

  an **`xds`** model object

- i:

  the vector species index

- options:

  a named list

## Value

an **`xds`** model object

## See also

[`get_XH_pars.SIS()`](https://dd-harp.github.io/ramp.xds/reference/get_XH_pars.SIS.md)
and the github.io essay on [SIS
Dynamics](https://dd-harp.github.io/ramp.xds/articles/human_sis.html)
