# Compute the steady states as a function of the daily EIR

This method dispatches on the type of `MY_obj`

## Usage

``` r
# S3 method for class 'basicM_ode'
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

a [list](https://rdrr.io/r/base/list.html)

## State Variables

- `M`:

  density of adult mosquitoes

- `P`:

  density of parous adult mosquitoes: those that have blood-fed at least
  once

## Dynamics

Adult mosquitoes emerge from aquatic habitats at rate \\(\Lambda\\) and
disperse and die according to \\(\Omega\\). The ODE model is \$\$
\begin{array}{rl} dM/dt &= \Lambda-\Omega\cdot M \\\\\\\\ dP/dt &=
f(M-P)-\Omega\cdot P \end{array}\$\$ Here, \\(P\\) is the density of
mosquitoes that have blood-fed at least once. Parity, \\(P/M\\), is
computed during parsing and stored as `parous`. For discrete-time
models,
[Update_MYt.basicM](https://dd-harp.github.io/ramp.xds/reference/Update_MYt.basicM.md)
uses the one-day update equations.

## See also

[basicM](https://dd-harp.github.io/ramp.xds/reference/basicM.md)
