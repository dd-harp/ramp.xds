# Update state variables for `basicL` (**L**)

This implements a discrete-time system for aquatic mosquito ecology.
This discrete-time system was adapted from a version published by Smith
DL, *et al.* (2013).

## Usage

``` r
# S3 method for class 'basicL'
Update_Lt(t, y, xds_obj, s)
```

## Arguments

- t:

  time

- y:

  dependent variables

- xds_obj:

  an **`xds`** model object

- s:

  the species index

## Value

a [numeric](https://rdrr.io/r/base/numeric.html) vector

## State Variables

- `L`:

  density of mosquito larvae in each habitat

## Parameters

- `psi` or \\(\psi\\):

  maturation rate

- `xi` or \\(\xi\\):

  delayed maturation parameter in response to mean crowding

- `phi` or \\(\phi\\):

  density-independent mortality rate

- `theta` or \\(\theta\\):

  slope of density-dependent mortality

## DTS

The one-day update in Update_Lt.basicL is: \$\$L_t = \eta + e^{-(\psi
e^{-\xi L\_{t-1}} + \phi + \theta L\_{t-1})} L\_{t-1}\$\$

## References

Smith DL, Perkins TA, Tusting LS, Scott TW, Lindsay SW (2013). “Mosquito
population regulation and larval source management in heterogeneous
environments.” *PLoS ONE*, **8**(8), e71247.
[doi:10.1371/journal.pone.0071247](https://doi.org/10.1371/journal.pone.0071247)
.
