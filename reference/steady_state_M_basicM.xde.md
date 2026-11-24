# Compute steady states for `basicM` (**MY**)

Compute steady-state mosquito densities for the `basicM` module given
adult mosquito emergence `Lambda` for the differential-equation system.

## Usage

``` r
# S3 method for class 'xde'
steady_state_M_basicM(Lambda, xds_obj, s = 1)
```

## Arguments

- Lambda:

  the daily emergence rate of adult mosquitoes

- xds_obj:

  an **`xds`** model object

- s:

  the vector species index

## Value

a named [numeric](https://rdrr.io/r/base/numeric.html) vector containing
`M` followed by `P`, each with one value per patch

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## ODE

Adult mosquitoes emerge from aquatic habitats at rate \\\Lambda\\ and
disperse and die according to \\\Omega\\. The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\P\\ is the density of
mosquitoes that have blood-fed at least once.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md)
