# Compute observed prevalence

Return observed prevalence for the `trivialXH` module as `q` times the
true prevalence returned by
[F_prevalence](https://dd-harp.github.io/ramp.xds/reference/F_prevalence.md).
True prevalence is empty for this module, so observed prevalence is
empty too.

## Usage

``` r
# S3 method for class 'trivialXH'
F_pr_obs(vars, XH_obj)
```

## Arguments

- vars:

  a list with the variables attached by name

- XH_obj:

  a list defining a model for humans

## Value

an empty [numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

[trivialXH](https://dd-harp.github.io/ramp.xds/reference/trivialXH.md)
