# Compute steady states for `basicL` (**L**)

Select the equation-system-specific steady-state method for `basicL`.

## Usage

``` r
# S3 method for class 'basicL'
steady_state_L(eta, xds_obj, s = 1)
```

## Arguments

- eta:

  the number of eggs laid in each habitat, per day

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a named [list](https://rdrr.io/r/base/list.html) containing the values
of \\(L\\) at the steady state

## See also

[steady_state_L_basicL](https://dd-harp.github.io/ramp.xds/reference/steady_state_L_basicL.md)
