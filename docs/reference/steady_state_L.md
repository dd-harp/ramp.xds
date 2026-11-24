# Compute steady states (**L**)

Compute steady states as a function of the number of eggs laid in each
habitat, per day, \\(\eta\\).

## Usage

``` r
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

a named [list](https://rdrr.io/r/base/list.html): values of the state
variables at the steady state
