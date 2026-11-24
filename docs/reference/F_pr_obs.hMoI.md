# Compute observed prevalence

Return apparent prevalence for the `hMoI` module as `1 - exp(-m2)`,
where `m2` is the mean MoI for apparent infections.

## Usage

``` r
# S3 method for class 'hMoI'
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

[hMoI](https://dd-harp.github.io/ramp.xds/reference/hMoI.md)
