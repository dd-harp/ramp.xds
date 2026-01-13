# Compute steady states for **MY**

Given the emergence rate of adult mosquitoes (\\\Lambda\\ or `Lambda`)
and the net infectiousness of humans (\\\kappa\\ or `kappa`), compute
the steady states for the system

## Usage

``` r
steady_state_MY(Lambda, kappa, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- kappa:

  net infectiousness

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

the steady states
