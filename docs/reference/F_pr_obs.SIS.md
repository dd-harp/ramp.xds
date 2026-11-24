# Compute observed prevalence

Return observed prevalence for the `SIS` module as `q * I / H`, where
`q` is test positivity.

## Usage

``` r
# S3 method for class 'SIS'
F_pr_obs(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for humans

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector of length
`nStrata`

## See also

[SIS](https://dd-harp.github.io/ramp.xds/reference/SIS.md)
