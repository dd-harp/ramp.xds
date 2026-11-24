# Compute steady states for `basicM` (**MY**)

Select the equation-system-specific steady-state method for `basicM`.

## Usage

``` r
# S3 method for class 'basicM'
steady_state_M(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the steady states as a named
[numeric](https://rdrr.io/r/base/numeric.html) vector

## See also

[steady_state_M_basicM](https://dd-harp.github.io/ramp.xds/reference/steady_state_M_basicM.md)
