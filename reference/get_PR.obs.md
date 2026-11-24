# Get observed prevalence

Return observed prevalence using
[F_pr_obs](https://dd-harp.github.io/ramp.xds/reference/F_pr_obs.md).

## Usage

``` r
# S3 method for class 'obs'
get_PR(xds_obj, method = "obs", i = 1)
```

## Arguments

- xds_obj:

  an **`xds`** model object

- method:

  the method used for computing *Pf*PR

- i:

  the host species index

## Value

a [list](https://rdrr.io/r/base/list.html) containing `time`, `pr`, and
`method`
